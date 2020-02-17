// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.rxjava

import com.digitalasset.grpc.{GrpcException, GrpcStatus}
import org.scalatest.{Assertion, Matchers}

private[rxjava] trait AuthMatchers { self: Matchers =>

  private def theCausalChainOf(t: Throwable): Iterator[Throwable] =
    Iterator.iterate(t)(_.getCause).takeWhile(_ != null)

  private def expectError(predicate: Throwable => Boolean)(call: => Any): Assertion =
    theCausalChainOf(the[RuntimeException] thrownBy call).filter(predicate) should not be empty

  def expectUnauthenticated(call: => Any): Assertion =
    expectError {
      case GrpcException(GrpcStatus.UNAUTHENTICATED(), _) => true
      case _ => false
    }(call)

  def expectPermissionDenied(call: => Any): Assertion =
    expectError {
      case GrpcException(GrpcStatus.PERMISSION_DENIED(), _) => true
      case _ => false
    }(call)

}
