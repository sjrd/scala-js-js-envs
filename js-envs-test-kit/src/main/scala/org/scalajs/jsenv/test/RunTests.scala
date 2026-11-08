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

package org.scalajs.jsenv.test

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.google.common.jimfs.Jimfs

import org.junit.Assume._
import org.junit.{Test, AssumptionViolatedException}

import org.scalajs.ir.OriginalName.NoOriginalName

import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.{TestKit, Run}

import WasmModuleHelpers._

private[test] class RunTests(config: JSEnvSuiteConfig, withCom: Boolean,
    defaultInputKind: TestKit.InputKind) {
  import RunTests._

  private val kit = new TestKit(config.jsEnv, config.awaitTimeout, defaultInputKind)

  /** Whether the input kind is based on JavaScript, as opposed to Wasm.
   *
   *  Virtually all tests have a separate code path for JS and Wasm. We keep
   *  them in the same class and test functions regardless. This way, we can
   *  visually inspect that we test the same features for JS and Wasm.
   *
   *  JS tests often use `console.log` as additional confirmation that things
   *  happen. The Wasm version of those tests cannot do that, since there is
   *  no way to print. They only test success or failure.
   */
  private val isJS: Boolean = defaultInputKind != TestKit.InputKind.WasmModule

  private def withRun(input: Seq[Input])(body: Run => Unit): Unit = {
    if (withCom) kit.withComRun(input)(body)
    else kit.withRun(input)(body)
  }

  private def withRun(code: String, config: RunConfig = RunConfig())(body: Run => Unit): Unit = {
    if (withCom) kit.withComRun(code, config)(body)
    else kit.withRun(code, config)(body)
  }

  private def withRun(module: wamod.Module, config: RunConfig)(body: Run => Unit): Unit = {
    if (withCom) kit.withComRun(toWasmModule(module), config)(body)
    else kit.withRun(toWasmModule(module), config)(body)
  }

  private def withRun(module: wamod.Module)(body: Run => Unit): Unit =
    withRun(module, RunConfig())(body)

  private val emptyWasmModule = makeWasmModuleBuilder().build()

  @Test
  def failureTest: Unit = {
    if (isJS) {
      withRun("""
        var a = {};
        a.foo();
      """) {
        _.fails()
      }
    } else {
      val builder = makeWasmModuleBuilder()

      locally {
        val fb = newFunctionBuilder(builder, StartFunctionID)
        fb += wa.Unreachable // traps
        fb.buildAndAddToModule()
      }

      builder.setStart(StartFunctionID)

      withRun(builder.build()) {
        _.fails()
      }
    }
  }

  @Test
  def syntaxErrorTest: Unit = {
    if (isJS) {
      withRun("{") {
        _.fails()
      }
    } else {
      val builder = makeWasmModuleBuilder()

      locally {
        val fb = newFunctionBuilder(builder, StartFunctionID)
        fb += wa.I32Add // does not validate; there is nothing on the stack
        fb.buildAndAddToModule()
      }

      builder.setStart(StartFunctionID)

      withRun(builder.build()) {
        _.fails()
      }
    }
  }

  @Test
  def throwExceptionTest: Unit = {
    if (isJS) {
      withRun("throw 1;") {
        _.fails()
      }
    } else {
      val builder = makeWasmModuleBuilder()

      val payloadID = builder.functionTypeToTypeID(watpe.FunctionType(List(watpe.Int32), Nil))
      builder.addTag(wamod.Tag(SimpleExceptionTag, NoOriginalName, payloadID))

      locally {
        val fb = newFunctionBuilder(builder, StartFunctionID)
        fb += wa.I32Const(1)
        fb += wa.Throw(SimpleExceptionTag)
        fb.buildAndAddToModule()
      }

      builder.setStart(StartFunctionID)

      withRun(builder.build()) {
        _.fails()
      }
    }
  }

  @Test
  def catchExceptionTest: Unit = {
    if (isJS) {
      withRun("""
        try {
          throw "hello world";
        } catch (e) {
          console.log(e);
        }
      """) {
        _.expectOut("hello world\n")
          .closeRun()
      }
    } else {
      val builder = makeWasmModuleBuilder()

      val payloadID = builder.functionTypeToTypeID(watpe.FunctionType(List(watpe.Int32), Nil))
      builder.addTag(wamod.Tag(SimpleExceptionTag, NoOriginalName, payloadID))

      locally {
        val fb = newFunctionBuilder(builder, StartFunctionID)
        fb.block(watpe.Int32) { catchLabel =>
          fb.tryTable()(
            List(wa.CatchClause.Catch(SimpleExceptionTag, catchLabel))
          ) {
            fb += wa.I32Const(1)
            fb += wa.Throw(SimpleExceptionTag)
          }
          fb += wa.Unreachable
        }

        // do some stuff with the int, just for validation purposes
        fb += wa.I32Const(2)
        fb += wa.I32Add
        fb += wa.Drop

        fb.buildAndAddToModule()
      }

      builder.setStart(StartFunctionID)

      withRun(builder.build()) {
        _.closeRun()
      }
    }
  }

  @Test // Failed in Phantom - #2053
  def utf8Test: Unit = {
    assumeTrue("Irrelevant for Wasm", isJS)

    withRun("console.log('\u1234')") {
      _.expectOut("\u1234\n")
        .closeRun()
    }
  }

  @Test
  def allowScriptTags: Unit = {
    assumeTrue("Irrelevant for Wasm", isJS)

    withRun("""console.log("<script></script>");""") {
      _.expectOut("<script></script>\n")
        .closeRun()
    }
  }

  @Test
  def jsExitsTest: Unit = {
    assumeTrue("Irrelevant for Wasm", isJS)

    val exitStat = config.exitJSStatement.getOrElse(
      throw new AssumptionViolatedException("JSEnv needs exitJSStatement"))

    withRun(exitStat) {
      _.succeeds()
    }
  }

  // #500 Node.js used to strip double percentage signs even with only 1 argument
  @Test
  def percentageTest: Unit = {
    assumeTrue("Irrelevant for Wasm", isJS)

    val strings = (1 to 15).map("%" * _)
    val code = strings.map(str => s"""console.log("$str");\n""").mkString("")
    val result = strings.mkString("", "\n", "\n")

    withRun(code) {
      _.expectOut(result)
        .closeRun()
    }
  }

  @Test
  def fastCloseTest: Unit = {
    /* This test also tests a failure mode where the ExternalJSRun is still
     * piping output while the client calls close.
     */
    if (isJS) {
      withRun("") {
        _.closeRun()
      }
    } else {
      withRun(emptyWasmModule) {
        _.closeRun()
      }
    }
  }

  @Test
  def multiCloseAfterTerminatedTest: Unit = {
    if (isJS) {
      withRun("") { run =>
        run.closeRun()

        // Should be noops (and not fail).
        run.closeRun()
        run.closeRun()
        run.closeRun()
      }
    } else {
      withRun(emptyWasmModule) { run =>
        run.closeRun()

        // Should be noops (and not fail).
        run.closeRun()
        run.closeRun()
        run.closeRun()
      }
    }
  }

  @Test
  def noThrowOnBadFileTest: Unit = {
    val badFile = Jimfs.newFileSystem().getPath("nonexistent")

    // `start` may not throw but must fail asynchronously
    withRun(kit.pathToInput(badFile) :: Nil) {
      _.fails()
    }
  }

  @Test
  def defaultFilesystem: Unit = {
    // Tests that a JSEnv works with files from the default filesystem.

    val tmpFile = File.createTempFile("sjs-run-test-defaultfile", if (isJS) ".js" else ".wasm")
    try {
      val tmpPath = tmpFile.toPath

      if (isJS) {
        Files.write(tmpPath, "console.log(\"test\");".getBytes(StandardCharsets.UTF_8))

        withRun(kit.pathToInput(tmpPath) :: Nil) {
          _.expectOut("test\n")
            .closeRun()
        }
      } else {
        toWasmModule(emptyWasmModule).writeTo(tmpPath)

        withRun(kit.pathToInput(tmpPath) :: Nil) {
          _.closeRun()
        }
      }
    } finally {
      tmpFile.delete()
    }
  }

  /* This test verifies that a [[JSEnv]] properly validates its [[RunConfig]]
   * (through [[RunConfig.Validator]]).
   *
   * If you get here, because the test suite fails on your [[JSEnv]] you are not
   * using [[RunConfig.Validator]] properly (or at all). See its documentation
   * on how to use it properly.
   *
   * This test sets a private option on [[RunConfig]] that is only known
   * internally. This ensures that [[JSEnv]]s reject options added in the future
   * they cannot support.
   */
  @Test(expected = classOf[IllegalArgumentException])
  def ensureValidate: Unit = {
    val cfg = RunConfig().withEternallyUnsupportedOption(true)
    if (isJS)
      withRun("", cfg)(identity)
    else
      withRun(emptyWasmModule, cfg)(identity)
  }

  @Test(expected = classOf[UnsupportedInputException])
  def ensureValidateInput: Unit = {
    val input = EternallyUnsupportedInput()
    withRun(Seq(input))(identity)
  }
}

private[test] object RunTests {
  final case class EternallyUnsupportedInput() extends Input

  private case object StartFunctionID extends wanme.FunctionID

  private case object SimpleExceptionTag extends wanme.TagID
}
