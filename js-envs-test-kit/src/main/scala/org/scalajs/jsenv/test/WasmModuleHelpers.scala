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

import scala.collection.mutable

import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position.NoPosition

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

import org.scalajs.jsenv.test.kit.WasmModule

/** Helpers to manipulate Wasm modules. */
private[test] object WasmModuleHelpers {
  private final case class FunctionTypeID(index: Int) extends TypeID

  /* TODO Remove this in favor of Scala.js 1.23.0 ModuleBuilder, which
   * integrates the logic for function type de-duplication.
   * See https://github.com/scala-js/scala-js/commit/27c2ddcbad65a238ffd84ace8a56e3363be2fb75
   */
  def makeWasmModuleBuilder(): ModuleBuilder = {
    val functionTypes = mutable.HashMap.empty[FunctionType, TypeID]

    lazy val moduleBuilder: ModuleBuilder = {
      new ModuleBuilder(new ModuleBuilder.FunctionTypeProvider {
        def functionTypeToTypeID(sig: FunctionType): TypeID = {
          functionTypes.getOrElseUpdate(
            sig, {
              val typeID = FunctionTypeID(functionTypes.size)
              moduleBuilder.addRecType(typeID, NoOriginalName, sig)
              typeID
            }
          )
        }
      })
    }

    moduleBuilder
  }

  def newFunctionBuilder(builder: ModuleBuilder, functionID: FunctionID): FunctionBuilder =
    new FunctionBuilder(builder, functionID, NoOriginalName, NoPosition)

  def toWasmModule(module: Module): WasmModule =
    WasmModule(BinaryWriter.write(module, emitDebugInfo = false))
}
