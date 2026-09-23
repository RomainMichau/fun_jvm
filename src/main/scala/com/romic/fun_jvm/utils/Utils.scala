package com.romic.fun_jvm.utils

import com.romic.fun_jvm.FValue

import java.nio.ByteBuffer
import scala.annotation.targetName

object Utils {

  val primitiveNames: Set[String] =
    Set("boolean", "byte", "char", "short", "int", "long", "float", "double", "void")

  def int2Bytes(int: Int): Array[Byte] =
    ByteBuffer.allocate(4).putInt(int).array

  def long2Bytes(long: Long): Array[Byte] =
    ByteBuffer.allocate(8).putLong(long).array

  def short2Bytes(short: Short): Array[Byte] =
    ByteBuffer.allocate(2).putShort(short).array

  def byte2Bytes(byte: Byte): Array[Byte] =
    Array(byte)

  def float2Bytes(float: Float): Array[Byte] =
    ByteBuffer.allocate(4).putFloat(float).array

  def double2Bytes(double: Double): Array[Byte] =
    ByteBuffer.allocate(8).putDouble(double).array

  def char2Bytes(char: Char): Array[Byte] =
    ByteBuffer.allocate(2).putChar(char).array

  def boolean2Bytes(bool: Boolean): Array[Byte] =
    Array(if bool then 1.toByte else 0.toByte)

  def bytes2Int(bytes: Array[Byte]): Int =
    ByteBuffer.wrap(bytes).getInt

  def bytes2Long(bytes: Array[Byte]): Long =
    ByteBuffer.wrap(bytes).getLong

  def bytes2Short(bytes: Array[Byte]): Short =
    ByteBuffer.wrap(bytes).getShort

  def bytes2Byte(bytes: Array[Byte]): Byte =
    bytes(0)

  def bytes2Float(bytes: Array[Byte]): Float =
    ByteBuffer.wrap(bytes).getFloat

  def bytes2Double(bytes: Array[Byte]): Double =
    ByteBuffer.wrap(bytes).getDouble

  def bytes2Char(bytes: Array[Byte]): Char =
    ByteBuffer.wrap(bytes).getChar

  def bytes2Boolean(bytes: Array[Byte]): Boolean =
    bytes(0) != 0

  extension (i: Int) def toFValue: FValue.FValueInt = FValue.FValueInt(i)

  extension (l: Long) def toFValue: FValue.FValueLong = FValue.FValueLong(l)

  extension (s: Short) def toFValue: FValue.FValueShort = FValue.FValueShort(s)

  extension (f: Float) def toFValue: FValue.FValueFloat = FValue.FValueFloat(f)

  extension (d: Double) def toFValue: FValue.FValueDouble = FValue.FValueDouble(d)

  extension (c: Char) def toFValue: FValue.FValueChar = FValue.FValueChar(c)

  extension (bo: Boolean) def toFValue: FValue.FValueBoolean = FValue.FValueBoolean(bo)

  case class ByteRange(from: Byte, to: Byte) {
    def contains(b: Byte): Boolean = b >= from && b <= to
  }

  extension (b: Byte) {
    infix def to(b2: Byte): ByteRange = ByteRange(b, b2)

    def toInt0Ext: Int = b & 0xff
    def toUShort: UShort = UShort(b & 0xff)
    def toUByte: UByte = UByte(b & 0xff)
    def toFValue: FValue.FValueByte = FValue.FValueByte(b)
  }

  object UShort {

    def apply(i: Int): UShort = {
      if i < 0 || i > 65535 then throw new IllegalArgumentException(s"$i is not unsigned short")
      i
    }

  }

  opaque type UShort = Int

  extension (u: UShort) {
    def toInt: Int = u
  }

  object UByte {

    def apply(i: Int): UByte = {
      if i < 0 || i > 255 then throw new IllegalArgumentException(s"$i is not unsigned byte")
      i
    }

  }

  opaque type UByte = Int

  extension (u: UByte) {

    @targetName("uByteToInt")
    def toInt: Int = u

  }

  object UInt {

    def apply(i: Long): UInt = {
      if i < 0 || i > 0xffffffffL then throw new IllegalArgumentException(s"$i is not unsigned int")
      i
    }

  }

  opaque type UInt = Long

  extension (u: UInt) {
    def toLong: Long = u
    def toInt: Int = u.toInt
  }

}
