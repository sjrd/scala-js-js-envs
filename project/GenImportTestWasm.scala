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

package build

import java.io.File

import scala.collection.mutable

import sbt.IO

import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position.NoPosition

import org.scalajs.linker.backend.webassembly._
import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Instructions._
import org.scalajs.linker.backend.webassembly.Modules._
import org.scalajs.linker.backend.webassembly.Types._

object GenImportTestWasm {
  def generate(target: File, relPathToTestLib: String): Unit = {
    val moduleBuilder = makeWasmModuleBuilder()

    // import { add32, printint } from '<relPathToTestLib>';
    moduleBuilder.addImport(Import(
      relPathToTestLib,
      "add32",
      ImportDesc.Func(
        funcs.add32,
        NoOriginalName,
        moduleBuilder.functionTypeToTypeID(FunctionType(List(Int32, Int32), List(Int32)))
      )
    ))
    moduleBuilder.addImport(Import(
      relPathToTestLib,
      "printint",
      ImportDesc.Func(
        funcs.printint,
        NoOriginalName,
        moduleBuilder.functionTypeToTypeID(FunctionType(List(Int32), Nil))
      )
    ))

    // import { totalmem } from 'node:os';
    moduleBuilder.addImport(Import(
      "node:os",
      "totalmem",
      ImportDesc.Func(
        funcs.totalmem,
        NoOriginalName,
        moduleBuilder.functionTypeToTypeID(FunctionType(Nil, List(Float64)))
      )
    ))

    // start function
    locally {
      val fb = new FunctionBuilder(moduleBuilder, funcs.start, NoOriginalName, NoPosition)

      // printint(add32(13, 29));
      fb += I32Const(13)
      fb += I32Const(29)
      fb += Call(funcs.add32)
      fb += Call(funcs.printint)

      // printint(totalmem() > (1024 * 1024) ? 1 : 0);
      fb += Call(funcs.totalmem)
      fb += F64Const(1024 * 1024)
      fb += F64Gt
      fb += Call(funcs.printint)

      fb.buildAndAddToModule()
    }

    moduleBuilder.setStart(funcs.start)

    // Build and write the module to the target file
    val module = moduleBuilder.build()
    val moduleByteBuffer = BinaryWriter.write(module, emitDebugInfo = false)
    val moduleBytes = new Array[Byte](moduleByteBuffer.remaining())
    moduleByteBuffer.get(moduleBytes)
    IO.write(target, moduleBytes)
  }

  private final case class SimpleFunctionID(name: String) extends FunctionID

  private object funcs {
    val add32 = SimpleFunctionID("add32")
    val printint = SimpleFunctionID("printint")
    val totalmem = SimpleFunctionID("totalmem")
    val start = SimpleFunctionID("start")
  }

  private final case class FunctionTypeID(index: Int) extends TypeID

  /* TODO Remove this in favor of Scala.js 1.23.0 ModuleBuilder, which
   * integrates the logic for function type de-duplication.
   * See https://github.com/scala-js/scala-js/commit/27c2ddcbad65a238ffd84ace8a56e3363be2fb75
   */
  private def makeWasmModuleBuilder(): ModuleBuilder = {
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
}
