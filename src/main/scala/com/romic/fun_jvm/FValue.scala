package com.romic.fun_jvm

import java.nio.ByteBuffer
import FValue.FValueInt

object FValue {
  extension (i: FValueInt) {
    def +(i2: FValueInt): FValueInt = FValueInt(i.value + i2.value)
  }

  extension (i: FValueReference) {
    def toHeapAddr: Heap.Address = Heap.Address(i.value.getOrElse(0))
  }

  object FValueReference {
    def null_ : FValueReference = FValueReference(None)

    def of(int: Int): FValueReference = FValueReference(Some(int))
  }


  def default(v: FType): FValue = v match {
    case FType.FTypeLong => FValueLong(0)
    case FType.FTypeInt => FValueInt(0)
    case FType.FTypeShort => FValueShort(0)
    case FType.FTypeByte => FValueByte(0)
    case FType.FTypeFloat => FValueFloat(0)
    case FType.FTypeDouble => FValueDouble(0)
    case FType.FTypeReference(_) => FValueReference.null_
    case FType.FTypeArray(_) => FValueArray(Array.empty[FValue])
    case FType.FTypeChar => FValueChar('\u0000')
    case FType.FTypeBoolean => FValueBoolean(false)
    case FType.Void => throw new RuntimeException("void has no default value")
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
      case ref if ref.startsWith("[") => FValueArray(Array.empty)
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
    case FValueArray(v) => v.flatMap(_.toBytes)

  case FValueLong(value: Long)
  case FValueInt(value: Int)
  case FValueShort(value: Short)
  case FValueByte(value: Byte)
  case FValueFloat(value: Float)
  case FValueDouble(value: Double)
  case FValueReference(value: Option[Int])
  case FValueChar(v: Char)
  case FValueBoolean(v: Boolean)
  case FValueArray[A <: FValue](v: Array[A])