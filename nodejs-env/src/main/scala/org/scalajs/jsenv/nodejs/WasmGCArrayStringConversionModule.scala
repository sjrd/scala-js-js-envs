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

import scala.collection.mutable

/** WebAssembly module that exposes functions to convert GC arrays to/from JS strings.
 *
 *  The module exports 2 functions:
 *
 *  - `stringToArray: [externref] -> [(ref (array (mut i16)))]`
 *  - `arrayToString: [(ref (array (mut i16)))] -> [externref]`
 */
private[nodejs] object WasmGCArrayStringConversionModule {

  val moduleBytes: Array[Byte] =
    makeModuleBytes()

  private final case class FuncDesc(
    exportName: Option[String],
    paramTypes: Seq[Seq[Byte]],
    resultTypes: Seq[Seq[Byte]],
    impl: FuncImpl
  )

  private sealed abstract class FuncImpl

  private final case class ImportImpl(moduleName: String, name: String) extends FuncImpl

  private final case class BodyImpl(localTypes: Seq[Seq[Byte]], body: Seq[Byte]) extends FuncImpl

  private def makeModuleBytes(): Array[Byte] = {
    val buf = new mutable.ArrayBuffer[Byte]()

    // magic header: null char + "asm"
    buf ++= Seq(0, 'a', 's', 'm')

    // version
    buf ++= Seq(1, 0, 0, 0)

    val i32Code = 0x7f.toByte
    val i16Code = 0x77.toByte
    val externrefCode = 0x6f.toByte

    val NullableHeapTypePrefix = 0x63.toByte
    val NonNullableHeapTypePrefix = 0x64.toByte

    val i32Type = Seq(i32Code)
    val i16Type = Seq(i16Code)
    val externrefType = Seq(externrefCode)
    val externType = Seq(NonNullableHeapTypePrefix, externrefCode)

    val arrayTypeID = 0.toByte
    val arrayType = Seq(NonNullableHeapTypePrefix, arrayTypeID)
    val nullableArrayType = Seq(NullableHeapTypePrefix, arrayTypeID)

    val functions: Seq[FuncDesc] = Seq(
      FuncDesc(
        None,
        Seq(externrefType),
        Seq(i32Type),
        ImportImpl("wasm:js-string", "length"),
      ),
      FuncDesc(
        None,
        Seq(externrefType, nullableArrayType, i32Type),
        Seq(i32Type),
        ImportImpl("wasm:js-string", "intoCharCodeArray"),
      ),
      FuncDesc(
        None,
        Seq(nullableArrayType, i32Type, i32Type),
        Seq(externType),
        ImportImpl("wasm:js-string", "fromCharCodeArray"),
      ),
      FuncDesc(
        Some("stringToArray"),
        Seq(externrefType),
        Seq(arrayType),
        BodyImpl(
          Seq(
            arrayType, // $result
          ),
          Seq(
            0x20, 0, // local.get $param (for call of $intoCharCodeArray)
            0x20, 0, // local.get $param (for call of $length)
            0x10, 0, // call $length
            0xfb.toByte, 0x07, arrayTypeID, // array.new_default $arrayTypeID
            0x22, 1, // local.tee $result
            0x41, 0, // i32.const 0
            0x10, 1, // call $intoCharCodeArray
            0x1a.toByte, // drop (the i32 returned by $intoCharCodeArray)
            0x20, 1, // local.get $result
          ),
        ),
      ),
      FuncDesc(
        Some("arrayToString"),
        Seq(arrayType),
        Seq(externrefType),
        BodyImpl(
          Nil,
          Seq(
            0x20, 0, // local.get $param
            0x41, 0, // i32.const 0
            0x20, 0, // local.get $param
            0xfb.toByte, 0x0f, // array.length
            0x10, 2, // call $fromCharCodeArray
          ),
        ),
      ),
    )

    def writeSection(sectionID: Byte)(sectionContent: => Unit): Unit = {
      buf += sectionID
      byteLengthSubSection(buf)(sectionContent)
    }

    def writeShortASCIIName(name: String): Unit = {
      assert(name.length() < 0x80)
      buf += name.length().toByte
      buf ++= name.map(_.toByte)
    }

    // Types section
    writeSection(sectionID = 0x01) {
      // number of types
      buf += (1 + functions.size).toByte

      // (type $theArray (array (mut $storageType))
      buf += 0x5e // array
      buf += i16Code
      buf += 1 // mut

      // function types
      for ((func, index) <- functions.zipWithIndex) {
        buf += 0x60 // func
        buf += func.paramTypes.size.toByte
        for (paramType <- func.paramTypes)
          buf ++= paramType
        buf += func.resultTypes.size.toByte
        for (resultType <- func.resultTypes)
          buf ++= resultType
      }
    }

    // Import section
    writeSection(sectionID = 0x02) {
      buf += functions.count(_.impl.isInstanceOf[ImportImpl]).toByte

      for ((func, index) <- functions.zipWithIndex) {
        func.impl match {
          case ImportImpl(moduleName, name) =>
            writeShortASCIIName(moduleName)
            writeShortASCIIName(name)
            buf += 0x00 // func
            buf += (1 + index).toByte // the function's type ID

          case _: BodyImpl =>
            ()
        }
      }
    }

    // Function section
    writeSection(sectionID = 0x03) {
      buf += functions.count(_.impl.isInstanceOf[BodyImpl]).toByte

      for ((func, index) <- functions.zipWithIndex) {
        if (func.impl.isInstanceOf[BodyImpl])
          buf += (1 + index).toByte // the function's type ID
      }
    }

    // Export section
    writeSection(sectionID = 0x07) {
      buf += functions.count(_.exportName.isDefined).toByte

      for {
        (func, index) <- functions.zipWithIndex
        exportName <- func.exportName
      } {
        writeShortASCIIName(exportName) // name
        buf += 0x00 // func
        buf += index.toByte // func ID
      }
    }

    // Code section
    writeSection(sectionID = 0x0a) {
      buf += functions.count(_.impl.isInstanceOf[BodyImpl]).toByte

      for (func <- functions) {
        func.impl match {
          case _: ImportImpl =>
            ()

          case BodyImpl(localTypes, body) =>
            byteLengthSubSection(buf) {
              buf += localTypes.size.toByte
              for (localType <- localTypes) {
                buf += 1 // 1 local of that type
                buf ++= localType
              }

              buf ++= body

              buf += 0x0b // end
            }
        }
      }
    }

    buf.toArray
  }

  // !!! Adapted from webassembly.BinaryWriter
  private def byteLengthSubSection(buf: mutable.ArrayBuffer[Byte])(subSectionContent: => Unit): Unit = {
    // Reserve 4 bytes at the current offset to store the byteLength later
    val byteLengthOffset = buf.size
    for (_ <- 0 until 4) // write fake bytes for now
      buf += 0
    val startOffset = buf.size

    subSectionContent

    // Compute byteLength
    val endOffset = buf.size
    val byteLength = endOffset - startOffset

    /* Write the byteLength in the reserved slot. Note that we *always* use
     * 4 bytes to store the byteLength, even when less bytes are necessary in
     * the unsigned LEB encoding. The WebAssembly spec specifically calls out
     * this choice as valid. We leverage it to have predictable total offsets
     * when we write the code section, which is important to efficiently
     * generate source maps.
     */
    buf(byteLengthOffset) = ((byteLength & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 1) = (((byteLength >>> 7) & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 2) = (((byteLength >>> 14) & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 3) = ((byteLength >>> 21) & 0x7f).toByte
  }
}
