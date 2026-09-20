package com.romic.fun_jvm

import java.nio.ByteBuffer
import FValue.FValueInt

object FValue {
  extension (i: FValueInt) {
    def +(i2: FValueInt): FValueInt = FValueInt(i.v + i2.v)
  }

  object FValueReference {
    def null_ : FValueReference = FValueReference(None)

    def of(int: Int): FValueReference = FValueReference(Some(int))
  }

  def initFromDescriptor(desc: String): FValue = {
    desc match {
      case "B" => FValueByte(0)
      case "C" => FValueChar('\u0000')
      case "D" => FValueDouble(0)
      case "F" => FValueFloat(0)
      case "I" => FValueInt(0)
      case "J" => FValueLong(0)
      case "S" => FValueShort(0)
      case "Z" => FValueBoolean(false)
      case ref if ref.startsWith("L") => FValueReference.null_
    }
  }
}

enum FValue:
  def toBytes: Array[Byte] = this match
    case FValueLong(v) => ByteBuffer.allocate(8).putLong(v).array()
    case FValueInt(v) => ByteBuffer.allocate(4).putInt(v).array()
    case FValueShort(v) => ByteBuffer.allocate(2).putShort(v).array()
    case FValueByte(v) => Array(v)
    case FValueFloat(v) => ByteBuffer.allocate(4).putFloat(v).array()
    case FValueDouble(v) => ByteBuffer.allocate(8).putDouble(v).array()
    case FValueReference(v) => ByteBuffer.allocate(4).putInt(v.getOrElse(0)).array()
    case FValueChar(v) => ByteBuffer.allocate(2).putChar(v).array()
    case FValueBoolean(v) => Array(if v then 1.toByte else 0.toByte)

  case FValueLong(v: Long)
  case FValueInt(v: Int)
  case FValueShort(v: Short)
  case FValueByte(v: Byte)
  case FValueFloat(v: Float)
  case FValueDouble(v: Double)
  case FValueReference(v: Option[Int])
  case FValueChar(v: Char)
  case FValueBoolean(v: Boolean)