// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.participant.state.kvutils.committing

import com.codahale.metrics
import com.codahale.metrics.{Counter, Timer}
import com.daml.ledger.participant.state.kvutils.Conversions.{buildTimestamp, commandDedupKey, _}
import com.daml.ledger.participant.state.kvutils.DamlKvutils._
import com.daml.ledger.participant.state.kvutils.{Conversions, DamlStateMap, Err, InputsAndEffects}
import com.daml.ledger.participant.state.v1.{Configuration, ParticipantId, RejectionReason}
import com.digitalasset.daml.lf.archive.Decode
import com.digitalasset.daml.lf.archive.Reader.ParseError
import com.digitalasset.daml.lf.data.Ref.{PackageId, Party}
import com.digitalasset.daml.lf.data.Time.Timestamp
import com.digitalasset.daml.lf.engine.{Blinding, Engine}
import com.digitalasset.daml.lf.transaction.Node
import com.digitalasset.daml.lf.transaction.{BlindingInfo, GenTransaction}
import com.digitalasset.daml.lf.value.Value
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._

private[kvutils] case class ProcessTransactionSubmission(
    engine: Engine,
    entryId: DamlLogEntryId,
    recordTime: Timestamp,
    defaultConfig: Configuration,
    participantId: ParticipantId,
    txEntry: DamlTransactionEntry,
    // FIXME(JM): remove inputState as a global to avoid accessing it when the intermediate
    // state should be used.
    inputState: DamlStateMap) {

  import ProcessTransactionSubmission._
  import Common._
  import Commit._

  private implicit val logger: Logger =
    LoggerFactory.getLogger(this.getClass)

  def run: (DamlLogEntry, Map[DamlStateKey, DamlStateValue]) = Metrics.runTimer.time { () =>
    runSequence(
      inputState = inputState,
      "Authorize submitter" -> authorizeSubmitter,
      "Check Informee Parties Allocation" -> checkInformeePartiesAllocation,
      "Deduplicate" -> deduplicateCommand,
      "Validate Ledger Time" -> validateLedgerTime,
      "Validate Contract Key Uniqueness" -> validateContractKeyUniqueness,
      "Validate Model Conformance" -> timed(Metrics.interpretTimer, validateModelConformance),
      "Authorize and build result" -> authorizeAndBlind.flatMap(buildFinalResult)
    )
  }

  // -------------------------------------------------------------------------------

  private val (_, config) =
    Common.getCurrentConfiguration(defaultConfig, inputState, logger)

  private val txLet = parseTimestamp(txEntry.getLedgerEffectiveTime)
  private val submitterInfo = txEntry.getSubmitterInfo
  private val submitter = Party.assertFromString(submitterInfo.getSubmitter)
  private lazy val relTx = Conversions.decodeTransaction(txEntry.getTransaction)

  private val submissionSeedAndTime =
    Conversions
      .parseOptHash(txEntry.getSubmissionSeed)
      .map(_ -> Conversions.parseTimestamp(txEntry.getSubmissionTime))

  private def contractIsActiveAndVisibleToSubmitter(contractState: DamlContractState): Boolean = {
    val locallyDisclosedTo = contractState.getLocallyDisclosedToList.asScala
    val divulgedTo = contractState.getDivulgedToList.asScala
    val isVisible = locallyDisclosedTo.contains(submitter) || divulgedTo.contains(submitter)
    val isActive = {
      val activeAt = Option(contractState.getActiveAt).map(parseTimestamp)
      !contractState.hasArchivedAt && activeAt.exists(txLet >= _)
    }
    isVisible && isActive
  }

  // Pull all keys from referenced contracts. We require this for 'fetchByKey' calls
  // which are not evidenced in the transaction itself and hence the contract key state is
  // not included in the inputs.
  private lazy val knownKeys: Map[DamlContractKey, Value.AbsoluteContractId] =
    inputState.collect {
      case (key, Some(value))
          if value.hasContractState
            && value.getContractState.hasContractKey
            && contractIsActiveAndVisibleToSubmitter(value.getContractState) =>
        value.getContractState.getContractKey -> Conversions.stateKeyToContractId(key)
    }

  /** Deduplicate the submission. If the check passes we save the command deduplication
    * state.
    */
  private def deduplicateCommand: Commit[Unit] = {
    val dedupKey = commandDedupKey(submitterInfo)
    get(dedupKey).flatMap { dedupEntry =>
      if (dedupEntry.isEmpty) {
        Commit.set(
          dedupKey ->
            DamlStateValue.newBuilder
              .setCommandDedup(
                DamlCommandDedupValue.newBuilder
                  .setRecordTime(buildTimestamp(recordTime))
                  .build)
              .build)
      } else {
        logger.trace(
          s"Transaction rejected, duplicate command, correlationId=${txEntry.getSubmitterInfo.getCommandId}")
        reject(
          DamlTransactionRejectionEntry.newBuilder
            .setSubmitterInfo(txEntry.getSubmitterInfo)
            .setDuplicateCommand(Duplicate.newBuilder.setDetails("")))
      }
    }
  }

  /** Authorize the submission by looking up the party allocation and verifying
    * that the submitting party is indeed hosted by the submitting participant.
    *
    * If the "open world" setting is enabled we allow the submission even if the
    * party is unallocated.
    */
  private def authorizeSubmitter: Commit[Unit] =
    get(partyStateKey(submitter)).flatMap {
      case Some(partyAllocation) =>
        if (partyAllocation.getParty.getParticipantId == participantId)
          pass
        else
          reject(
            buildRejectionLogEntry(RejectionReason.SubmitterCannotActViaParticipant(
              s"Party '$submitter' not hosted by participant $participantId")))
      case None =>
        reject(buildRejectionLogEntry(RejectionReason.PartyNotKnownOnLedger))
    }

  /** Validate ledger effective time and the command's time-to-live. */
  private def validateLedgerTime: Commit[Unit] = delay {
    val timeModel = config.timeModel

    timeModel
      .checkTime(ledgerTime = txLet.toInstant, recordTime = recordTime.toInstant)
      .fold(
        reason => reject(buildRejectionLogEntry(RejectionReason.InvalidLedgerTime(reason))),
        _ => pass)
  }

  /** Validate the submission's conformance to the DAML model */
  private def validateModelConformance: Commit[Unit] = delay {
    val ctx = Metrics.interpretTimer.time()
    try {
      engine
        .validate(relTx, txLet, participantId, submissionSeedAndTime)
        .consume(lookupContract, lookupPackage, lookupKey)
        .fold(err => reject(buildRejectionLogEntry(RejectionReason.Disputed(err.msg))), _ => pass)
    } finally {
      val _ = ctx.stop()
    }
  }

  /** Validate the submission's conformance to the DAML model */
  private def authorizeAndBlind: Commit[BlindingInfo] = delay {
    Blinding
      .checkAuthorizationAndBlind(relTx, initialAuthorizers = Set(submitter))
      .fold(err => reject(buildRejectionLogEntry(RejectionReason.Disputed(err.msg))), pure)
  }

  private def validateContractKeyUniqueness: Commit[Unit] =
    for {
      damlState <- getDamlState
      startingKeys = damlState.collect {
        case (k, v) if k.hasContractKey && v.getContractKeyState.getContractId.nonEmpty => k
      }.toSet

      allUnique = relTx
        .fold((true, startingKeys)) {
          case (
              (allUnique, existingKeys),
              (_nodeId, exe @ Node.NodeExercises(_, _, _, _, _, _, _, _, _, _, _, _, _, _)))
              if exe.key.isDefined && exe.consuming =>
            val stateKey = Conversions.globalKeyToStateKey(
              Node.GlobalKey(exe.templateId, Conversions.forceNoContractIds(exe.key.get.key.value)))
            (allUnique, existingKeys - stateKey)

          case ((allUnique, existingKeys), (_nodeId, create @ Node.NodeCreate(_, _, _, _, _, _, _)))
              if create.key.isDefined =>
            val stateKey = Conversions.globalKeyToStateKey(
              Node.GlobalKey(
                create.coinst.template,
                Conversions.forceNoContractIds(create.key.get.key.value)))

            (allUnique && !existingKeys.contains(stateKey), existingKeys + stateKey)

          case (accum, _) => accum
        }
        ._1

      r <- if (allUnique)
        pass
      else
        reject(
          buildRejectionLogEntry(RejectionReason.Disputed("DuplicateKey: Contract Key not unique")))
    } yield r

  /** Check that all informee parties mentioned of a transaction are allocated. */
  private def checkInformeePartiesAllocation: Commit[Unit] = {

    def foldInformeeParties[T](tx: GenTransaction.WithTxValue[_, _], init: T)(
        f: (T, String) => T): T =
      tx.fold(init) {
        case (accum, (_, node)) =>
          node.informeesOfNode.foldLeft(accum)(f)
      }

    for {
      allExist <- foldInformeeParties(relTx, pure(true)) { (accum, party) =>
        get(partyStateKey(party)).flatMap(_.fold(pure(false))(_ => accum))
      }

      result <- if (allExist)
        pass
      else
        reject(
          buildRejectionLogEntry(RejectionReason.PartyNotKnownOnLedger)
        )
    } yield result
  }

  /** All checks passed. Produce the log entry and contract state updates. */
  private def buildFinalResult(blindingInfo: BlindingInfo): Commit[Unit] = delay {
    val effects = InputsAndEffects.computeEffects(relTx)

    val cid2nid: Value.AbsoluteContractId => Value.NodeId = relTx.localContracts

    // Helper to read the _current_ contract state.
    // NOTE(JM): Important to fetch from the state that is currently being built up since
    // we mark some contracts as archived and may later change their disclosure and do not
    // want to "unarchive" them.
    def getContractState(key: DamlStateKey): Commit[DamlContractState] =
      get(key).map {
        _.getOrElse(throw Err.MissingInputState(key)).getContractState
      }

    sequence2(
      // Add contract state entries to mark contract activeness (checked by 'validateModelConformance')
      set(effects.createdContracts.map {
        case (key, createNode) =>
          val cs = DamlContractState.newBuilder
          cs.setActiveAt(buildTimestamp(txLet))
          val localDisclosure =
            blindingInfo.localDisclosure(cid2nid(decodeContractId(key.getContractId)))
          cs.addAllLocallyDisclosedTo((localDisclosure: Iterable[String]).asJava)
          cs.setContractInstance(
            Conversions.encodeContractInstance(createNode.coinst)
          )
          createNode.key.foreach { keyWithMaintainers =>
            cs.setContractKey(
              Conversions.encodeGlobalKey(
                Node.GlobalKey
                  .build(
                    createNode.coinst.template,
                    keyWithMaintainers.key.value
                  )
                  .fold(
                    _ => throw Err.InvalidSubmission("Unexpected contract id in contract key."),
                    identity))
            )
          }
          key -> DamlStateValue.newBuilder.setContractState(cs).build
      }),
      // Update contract state entries to mark contracts as consumed (checked by 'validateModelConformance')
      sequence2(effects.consumedContracts.map { key =>
        for {
          cs <- getContractState(key).map { cs =>
            cs.toBuilder
              .setArchivedAt(buildTimestamp(txLet))
              .setArchivedByEntry(entryId)
          }
          r <- set(key -> DamlStateValue.newBuilder.setContractState(cs).build)
        } yield r
      }: _*),
      // Update contract state of divulged contracts
      sequence2(blindingInfo.globalDivulgence.map {
        case (coid, parties) =>
          val key = contractIdToStateKey(coid)
          getContractState(key).flatMap { cs =>
            val divulged: Set[String] = cs.getDivulgedToList.asScala.toSet
            val newDivulgences: Set[String] = parties.toSet[String] -- divulged
            if (newDivulgences.isEmpty)
              pass
            else {
              val cs2 = cs.toBuilder
                .addAllDivulgedTo(newDivulgences.asJava)
              set(key -> DamlStateValue.newBuilder.setContractState(cs2).build)
            }
          }
      }.toList: _*),
      // Update contract keys
      set(effects.updatedContractKeys.map {
        case (key, contractKeyState) =>
          logger.trace(s"updating contract key $key to $contractKeyState")
          key ->
            DamlStateValue.newBuilder
              .setContractKeyState(
                DamlContractKeyState.newBuilder.setContractId(contractKeyState.fold("")(_.coid))
              )
              .build
      }),
      delay {
        Metrics.accepts.inc()
        logger.trace(
          s"Transaction accepted, correlationId=${txEntry.getSubmitterInfo.getCommandId}")
        done(
          DamlLogEntry.newBuilder
            .setRecordTime(buildTimestamp(recordTime))
            .setTransactionEntry(txEntry)
            .build
        )
      }
    )
  }

  // Helper to lookup contract instances. We verify the activeness of
  // contract instances here. Since we look up every contract that was
  // an input to a transaction, we do not need to verify the inputs separately.
  private def lookupContract(coid: Value.AbsoluteContractId) = {
    val stateKey = contractIdToStateKey(coid)
    for {
      // Fetch the state of the contract so that activeness and visibility can be checked.
      contractState <- inputState.get(stateKey).flatMap(_.map(_.getContractState)).orElse {
        logger.warn(
          s"Lookup contract failed, contractId=$coid correlationId=${txEntry.getSubmitterInfo.getCommandId}")
        throw Err.MissingInputState(stateKey)
      }
      if contractIsActiveAndVisibleToSubmitter(contractState)
      contract = Conversions.decodeContractInstance(contractState.getContractInstance)
    } yield contract
  }

  // Helper to lookup package from the state. The package contents
  // are stored in the [[DamlLogEntry]], which we find by looking up
  // the DAML state entry at `DamlStateKey(packageId = pkgId)`.
  private def lookupPackage(pkgId: PackageId) = {
    val stateKey = packageStateKey(pkgId)
    for {
      value <- inputState
        .get(stateKey)
        .flatten
        .orElse {
          logger.warn(
            s"Lookup package failed, package not found, packageId=$pkgId correlationId=${txEntry.getSubmitterInfo.getCommandId}")
          throw Err.MissingInputState(stateKey)
        }
      pkg <- value.getValueCase match {
        case DamlStateValue.ValueCase.ARCHIVE =>
          // NOTE(JM): Engine only looks up packages once, compiles and caches,
          // provided that the engine instance is persisted.
          try {
            Some(Decode.decodeArchive(value.getArchive)._2)
          } catch {
            case ParseError(err) =>
              logger.warn(
                s"Decode archive failed, packageId=$pkgId correlationId=${txEntry.getSubmitterInfo.getCommandId}")
              throw Err.DecodeError("Archive", err)
          }

        case _ =>
          val msg = s"value not a DAML-LF archive"
          logger.warn(
            s"Lookup package failed, $msg, packageId=$pkgId correlationId=${txEntry.getSubmitterInfo.getCommandId}")
          throw Err.DecodeError("Archive", msg)
      }

    } yield pkg
  }

  private def lookupKey(key: Node.GlobalKey): Option[Value.AbsoluteContractId] = {
    val stateKey = Conversions.globalKeyToStateKey(key)
    inputState
      .get(stateKey)
      .flatMap {
        _.flatMap { value =>
          for {
            contractId <- Some(value.getContractKeyState.getContractId)
              .filter(_.nonEmpty)
              .map(decodeContractId)
            contractStateKey = contractIdToStateKey(contractId)
            contractState <- inputState.get(contractStateKey).flatMap(_.map(_.getContractState))
            if contractIsActiveAndVisibleToSubmitter(contractState)
          } yield contractId
        }
      }
      // If the key was not in state inputs, then we look whether any of the accessed contracts
      // has the key we're looking for. This happens with "fetchByKey" where the key lookup
      // is not evidenced in the transaction. The activeness of the contract is checked when it is fetched.
      .orElse {
        knownKeys.get(stateKey.getContractKey)
      }
  }

  private def buildRejectionLogEntry(
      reason: RejectionReason): DamlTransactionRejectionEntry.Builder = {
    logger.trace(
      s"Transaction rejected, ${reason.description}, correlationId=${txEntry.getSubmitterInfo.getCommandId}")
    val builder = DamlTransactionRejectionEntry.newBuilder
    builder
      .setSubmitterInfo(txEntry.getSubmitterInfo)

    reason match {
      case RejectionReason.Inconsistent =>
        builder.setInconsistent(Inconsistent.newBuilder.setDetails(""))
      case RejectionReason.Disputed(disputeReason) =>
        builder.setDisputed(Disputed.newBuilder.setDetails(disputeReason))
      case RejectionReason.ResourcesExhausted =>
        builder.setResourcesExhausted(ResourcesExhausted.newBuilder.setDetails(""))
      case RejectionReason.MaximumRecordTimeExceeded =>
        builder.setMaximumRecordTimeExceeded(MaximumRecordTimeExceeded.newBuilder.setDetails(""))
      case RejectionReason.PartyNotKnownOnLedger =>
        builder.setPartyNotKnownOnLedger(PartyNotKnownOnLedger.newBuilder.setDetails(""))
      case RejectionReason.SubmitterCannotActViaParticipant(details) =>
        builder.setSubmitterCannotActViaParticipant(
          SubmitterCannotActViaParticipant.newBuilder
            .setDetails(details))
      case RejectionReason.InvalidLedgerTime(reason) =>
        builder.setInvalidLedgerTime(InvalidLedgerTime.newBuilder.setDetails(reason))
    }
    builder
  }

  private def reject[A](rejectionEntry: DamlTransactionRejectionEntry.Builder): Commit[A] = {

    Metrics.rejections(rejectionEntry.getReasonCase.getNumber).inc()

    Commit.done(
      DamlLogEntry.newBuilder
        .setRecordTime(buildTimestamp(recordTime))
        .setTransactionRejectionEntry(rejectionEntry)
        .build,
    )
  }
}

object ProcessTransactionSubmission {
  private[committing] object Metrics {
    //TODO: Replace with metrics registry object passed in constructor
    private val registry = metrics.SharedMetricRegistries.getOrCreate("kvutils")
    private val prefix = "kvutils.committer.transaction"
    val runTimer: Timer = registry.timer(s"$prefix.run_timer")
    val interpretTimer: Timer = registry.timer(s"$prefix.interpret_timer")
    val accepts: Counter = registry.counter(s"$prefix.accepts")
    val rejections: Map[Int, Counter] =
      DamlTransactionRejectionEntry.ReasonCase.values
        .map(v => v.getNumber -> registry.counter(s"$prefix.rejections_${v.name}"))
        .toMap
  }
}
