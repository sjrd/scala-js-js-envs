/*
 * Scala.js JS Envs (https://github.com/scala-js/scala-js-js-envs)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.jsenv

import org.junit.Test
import org.junit.Assert._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import scala.util.Failure

class ExternalJSRunTest {

  private def assertFails(future: Future[Unit])(
      pf: PartialFunction[Throwable, Unit]): Unit = {
    Await.ready(future, 1.seconds).value.get match {
      case Failure(t) if pf.isDefinedAt(t) => // OK

      case result =>
        result.get
        fail("run succeeded unexpectedly")
    }
  }

  private val silentConfig = {
    val runConfig = RunConfig()
      .withInheritOut(false)
      .withInheritErr(false)
      .withOnOutputStream((_, _) => ())

    ExternalJSRun.Config()
      .withRunConfig(runConfig)
  }

  @Test
  def nonExistentCommand: Unit = {
    val cmd = List("nonexistent-cmd")
    val run = ExternalJSRun.start(cmd, silentConfig) { _ =>
      fail("unexpected call to input")
    }

    assertFails(run.future) {
      case ExternalJSRun.FailedToStartException(`cmd`, _) => // OK
    }
  }

  @Test
  def failingCommand: Unit = {
    val run = ExternalJSRun.start(List("node", "non-existent-file.js"),
        silentConfig)(_.close())

    assertFails(run.future) {
      case ExternalJSRun.NonZeroExitException(_) => // OK
    }
  }

  @Test
  def abortedCommand: Unit = {
    val run = ExternalJSRun.start(List("node"), silentConfig) { _ =>
      // Do not close stdin so node keeps running
    }

    run.close()

    assertFails(run.future) {
      case ExternalJSRun.ClosedException() => // OK
    }
  }

  @Test
  def abortedCommandOK: Unit = {
    val config = silentConfig
      .withClosingFails(false)

    val run = ExternalJSRun.start(List("node"), config) { _ =>
      // Do not close stdin so node keeps running
    }

    run.close()
    Await.result(run.future, 1.second)
  }
}
