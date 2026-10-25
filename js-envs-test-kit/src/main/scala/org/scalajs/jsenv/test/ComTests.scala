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

import org.junit.{Test, AssumptionViolatedException}
import org.junit.Assume._

import org.scalajs.ir.OriginalName.NoOriginalName

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.{ComRun, TestKit, WasmModule}

import WasmModuleHelpers._

private[test] class ComTests(config: JSEnvSuiteConfig,
    defaultInputKind: TestKit.InputKind) {
  import ComTests._

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

  @Test
  def basicTest: Unit = {
    val body: ComRun => Unit = { run =>
      run.expectMsg("Hello World")

      for (i <- 0 to 10) {
        run
          .send(i.toString)
          .expectMsg(s"received: $i")
      }

      run.expectNoMsgs()
        .closeRun()
    }

    if (isJS) {
      kit.withComRun("""
        scalajsCom.init(function(msg) { scalajsCom.send("received: " + msg); });
        scalajsCom.send("Hello World");
      """)(body)
    } else {
      val builder = prepareWasmModuleBuilder()

      // receive function
      locally {
        val replyPrefix = "received: "

        val fb = newFunctionBuilder(builder, ReceiveFunctionID)
        val msg = fb.addParam("msg", RefArrayMutI16Type)
        val msgLen = fb.addLocal("msgLen", watpe.Int32)
        val reply = fb.addLocal("reply", RefArrayMutI16Type)

        fb += wa.LocalGet(msg)
        fb += wa.ArrayLen
        fb += wa.LocalTee(msgLen)

        // Allocate array for the reply
        fb += wa.I32Const(replyPrefix.length())
        fb += wa.I32Add
        fb += wa.ArrayNewDefault(ArrayMutI16TypeID)
        fb += wa.LocalSet(reply)

        // Fill in the prefix
        for ((c, i) <- replyPrefix.zipWithIndex) {
          fb += wa.LocalGet(reply)
          fb += wa.I32Const(i)
          fb += wa.I32Const(c.toInt)
          fb += wa.ArraySet(ArrayMutI16TypeID)
        }

        // Copy the incoming message after the prefix
        fb += wa.LocalGet(reply) // dest
        fb += wa.I32Const(replyPrefix.length()) // destOffset
        fb += wa.LocalGet(msg) // src
        fb += wa.I32Const(0) // srcOffset
        fb += wa.LocalGet(msgLen) // length
        fb += wa.ArrayCopy(ArrayMutI16TypeID, ArrayMutI16TypeID)

        // Call send()
        fb += wa.LocalGet(reply)
        fb += wa.Call(SendFunctionID)

        fb.buildAndAddToModule()
      }

      // start function
      locally {
        val fb = newFunctionBuilder(builder, StartFunctionID)
        for (c <- "Hello World")
          fb += wa.I32Const(c.toInt)
        fb += wa.ArrayNewFixed(ArrayMutI16TypeID, "Hello World".length())
        fb += wa.Call(SendFunctionID)
        fb.buildAndAddToModule()
      }

      builder.setStart(StartFunctionID)

      kit.withComRun(toWasmModule(builder.build()))(body)
    }
  }

  @Test
  def jsExitsOnMessageTest: Unit = {
    assumeTrue("Irrelevant on Wasm", isJS)

    val exitStat = config.exitJSStatement.getOrElse(
        throw new AssumptionViolatedException("JSEnv needs exitJSStatement"))

    kit.withComRun(s"""
      scalajsCom.init(function(msg) { $exitStat });
      for (var i = 0; i < 10; ++i)
        scalajsCom.send("msg: " + i);
      """) { run =>

      for (i <- 0 until 10)
        run.expectMsg(s"msg: $i")

      run
        .send("quit")
        .expectNoMsgs()
        .succeeds()
    }
  }

  @Test
  def multiEnvTest: Unit = {
    val optWasmModule: Option[WasmModule] = if (isJS) {
      None
    } else {
      val builder = prepareWasmModuleBuilder()

      locally {
        val fb = newFunctionBuilder(builder, ReceiveFunctionID)
        val msg = fb.addParam("msg", RefArrayMutI16Type)
        for (c <- "pong")
          fb += wa.I32Const(c.toInt)
        fb += wa.ArrayNewFixed(ArrayMutI16TypeID, "pong".length())
        fb += wa.Call(SendFunctionID)
        fb.buildAndAddToModule()
      }

      Some(toWasmModule(builder.build()))
    }

    val n = 10
    val runs = List.fill(5) {
      if (isJS) {
        kit.startWithCom("""
        scalajsCom.init(function(msg) {
          scalajsCom.send("pong");
        });
        """)
      } else {
        kit.startWithCom(optWasmModule.get)
      }
    }

    try {
      for (_ <- 0 until n) {
        runs.foreach(_.send("ping"))
        runs.foreach(_.expectMsg("pong"))
      }

      runs.foreach {
        _.expectNoMsgs()
          .closeRun()
      }
    } finally {
      runs.foreach(_.close())
    }
  }

  private def replyTest(msg: String) = {
    val body: ComRun => Unit = {
      _.send(msg)
        .expectMsg(msg)
        .expectNoMsgs()
        .closeRun()
    }

    if (isJS) {
      kit.withComRun("scalajsCom.init(scalajsCom.send);")(body)
    } else {
      val builder = prepareWasmModuleBuilder()

      locally {
        val fb = newFunctionBuilder(builder, ReceiveFunctionID)
        val msg = fb.addParam("msg", RefArrayMutI16Type)
        fb += wa.LocalGet(msg)
        fb += wa.Call(SendFunctionID)
        fb.buildAndAddToModule()
      }

      kit.withComRun(toWasmModule(builder.build()))(body)
    }
  }

  @Test
  def largeMessageTest: Unit = {
    /* 1MB data.
     * (i & 0x7f) limits the input to the ASCII repertoire, which will use
     * exactly 1 byte per Char in UTF-8. This restriction also ensures that we
     * do not introduce surrogate characters and therefore no invalid UTF-16
     * strings.
     */
    replyTest(new String(Array.tabulate(1024 * 1024)(i => (i & 0x7f).toChar)))
  }

  @Test
  def highCharTest: Unit = { // #1536
    replyTest("\uC421\u8F10\u0112\uFF32")
  }

  @Test
  def noInitTest: Unit = {
    val body: ComRun => Unit = {
      _.send("Dummy")
        .expectNoMsgs()
        .closeRun()
    }

    if (isJS) {
      kit.withComRun("")(body)
    } else {
      // For Wasm, having no `init` call corresponds to not exporting "scalajs:com/receive"
      val emptyWasmModule = toWasmModule(makeWasmModuleBuilder().build())
      kit.withComRun(emptyWasmModule)(body)
    }
  }

  @Test
  def separateComStdoutTest: Unit = {
    // Make sure that com and stdout do not interfere with each other.
    assumeTrue("No console on Wasm", isJS)

    kit.withComRun("""
      scalajsCom.init(function (msg) {
        console.log("got: " + msg)
      });
      console.log("a");
      scalajsCom.send("b");
      scalajsCom.send("c");
      console.log("d");
    """) {
      _.expectOut("a\n")
        .expectMsg("b")
        .expectMsg("c")
        .expectOut("d\n")
        .send("foo")
        .expectOut("got: foo\n")
        .closeRun()
    }
  }
}

private[test] object ComTests {
  private case object SendFunctionID extends wanme.FunctionID
  private case object ReceiveFunctionID extends wanme.FunctionID
  private case object StartFunctionID extends wanme.FunctionID

  private case object ArrayMutI16TypeID extends wanme.TypeID

  private val ArrayMutI16TypeDef = {
    watpe.SubType(ArrayMutI16TypeID, NoOriginalName,
        watpe.ArrayType(watpe.FieldType(watpe.Int16, isMutable = true)))
  }

  private val RefArrayMutI16Type = watpe.RefType(ArrayMutI16TypeID)

  private val SendReceiveFunctionType =
    watpe.FunctionType(List(RefArrayMutI16Type), Nil)

  private def prepareWasmModuleBuilder(
      importSend: Boolean = true,
      exportReceive: Boolean = true
  ): ModuleBuilder = {
    val builder = makeWasmModuleBuilder()

    builder.addRecType(ArrayMutI16TypeDef)

    if (importSend) {
      val sendTypeID = builder.functionTypeToTypeID(SendReceiveFunctionType)

      builder.addImport(wamod.Import(
        "scalajs:com",
        "send",
        wamod.ImportDesc.Func(SendFunctionID, NoOriginalName, sendTypeID)
      ))
    }

    if (exportReceive) {
      builder.addExport(wamod.Export(
        "scalajs:com/receive",
        wamod.ExportDesc.Func(ReceiveFunctionID)
      ))
    }

    builder
  }
}
