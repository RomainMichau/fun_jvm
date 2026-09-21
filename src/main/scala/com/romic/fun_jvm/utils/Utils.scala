package com.romic.fun_jvm.utils

import java.nio.ByteBuffer

object Utils {
  def int2Bytes(int: Int): Array[Byte] = {
    ByteBuffer.allocate(4).putInt(int).array
  }
  case class ByteRange(from: Byte, to: Byte) {
    def contains(b: Byte): Boolean = b >= from && b <= to
  }

  extension (b: Byte) {
    def to(b2: Byte): ByteRange = ByteRange(b, b2)

    def toInt0Ext: Int = b & 0xff
    def toUShort: UShort = UShort(b & 0xff)
  }

  object UShort {
    def apply(i: Int): UShort = {
      if (i < 0 || i > 65535) then throw new IllegalArgumentException(s"$i is not unsigned short")
      i
    }
  }

  opaque type UShort = Int
  extension (u: UShort) {
    def toInt: Int = u
  }

  object UInt {
    def apply(i: Long): UInt = {
      if (i < 0 || i > 0xFFFFFFFFL) then throw new IllegalArgumentException(s"$i is not unsigned int")
      i
    }
  }

  opaque type UInt = Long
  extension (u: UInt) {
    def toLong: Long = u
    def toInt: Int = u.toInt
  }

}