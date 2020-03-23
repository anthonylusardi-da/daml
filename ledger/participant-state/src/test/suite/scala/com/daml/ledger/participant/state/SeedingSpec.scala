// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.participant.state.v1

import org.scalatest.{Matchers, WordSpec}

class SeedingSpec extends WordSpec with Matchers {
  "StaticRandomSeedService" should {
    "return the same sequence of random numbers across multiple runs" in {
      val gen1 = new StaticRandomSeedService(17L)
      val gen2 = new StaticRandomSeedService(17L)

      val hashes1 = List.fill(100)(gen1.nextSeed())
      val hashes2 = List.fill(100)(gen2.nextSeed())
      hashes1 shouldEqual hashes2
    }
  }
}
