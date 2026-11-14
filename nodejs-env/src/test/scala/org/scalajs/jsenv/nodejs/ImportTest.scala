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

package org.scalajs.jsenv.nodejs

import java.nio.file.Paths

import scala.concurrent.duration._

import org.scalajs.jsenv.Input
import org.scalajs.jsenv.test.kit.TestKit

import org.junit.Assume._
import org.junit.Test

/** Tests that imports are supported for several kinds of modules,
 *  in particular Wasm modules.
 */
class ImportTest {
  private val jsEnv = new NodeJSEnv()

  private val kit =
    new TestKit(jsEnv, timeout = 1.minute, defaultInputKind = TestKit.InputKind.Script)

  private val base = Paths.get("./nodejs-env").toAbsolutePath()

  private val supportsWasm = System.getenv("SJS_NODE_NO_WASM_TEST") != "true"

  @Test
  def testCommonJSModule(): Unit =
    testWithInput(Input.CommonJSModule(base.resolve("testfiles/subdir/cjs-test.js")))

  @Test
  def testESModule(): Unit =
    testWithInput(Input.ESModule(base.resolve("testfiles/subdir/esm-test.mjs")))

  @Test
  def testWasmModule(): Unit = {
    assumeTrue("no support for Wasm", supportsWasm)
    testWithInput(Input.WasmModule(base.resolve("target/wasm-test.wasm")))
  }

  private def testWithInput(input: Input): Unit = {
    kit.withRun(Seq(input)) {
      _.expectOut("42\n") // 13 + 29
        .expectOut("1\n") // a reasonable environment has more than 1M of total mem
        .closeRun()
    }
  }
}
