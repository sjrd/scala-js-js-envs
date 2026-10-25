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

package org.scalajs.jsenv.test.kit

import java.nio.ByteBuffer
import java.nio.file.Path
import java.nio.file.Files

/** A binary Wasm module.
 *
 *  To give to methods of [[TestKit]] to start runs with a Wasm module, rather
 *  than JavaScript code.
 */
final class WasmModule(content: Array[Byte]) {
  override def toString(): String = "WasmModule(<content>)"

  def writeTo(path: Path): Unit =
    Files.write(path, content)
}

object WasmModule {
  /** Creates a [[WasmModule]] with the given binary content.
   *
   *  The `content` array is copied by this method.
   */
  def apply(content: Array[Byte]): WasmModule =
    new WasmModule(content.clone())

  /** Creates a [[WasmModule]] with the given binary content.
   *
   *  The `content` buffer is consumed by this method: after calling, it, its
   *  `position()` will equal its `limit()`.
   */
  def apply(content: ByteBuffer): WasmModule = {
    val bytes = new Array[Byte](content.remaining())
    content.get(bytes)
    new WasmModule(bytes)
  }
}
