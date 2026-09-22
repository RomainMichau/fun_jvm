package com.romic.fun_jvm

import java.nio.ByteBuffer
import FValue.FValueInt
import com.romic.fun_jvm.Clazz.ClassName

object FValue {

  extension (i: FValueInt) {
    def +(i2: FValueInt): FValueInt = FValueInt(i.value + i2.value)
  }

  extension (i: FValueReference) {
    def toHeapAddr: Heap.Address = Heap.Address(i.value.getOrElse(0))
  }

  object FValueReference {
    def null_ : FValueReference = FValueReference(None, "java/lang/Object")

    def of(int: Int, className: ClassName): FValueReference = FValueReference(Some(int), className)
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

}

enum FValue:

  def getType: FType = this match
    case FValueLong(_) => FType.FTypeLong
    case FValueInt(_) => FType.FTypeInt
    case FValueShort(_) => FType.FTypeShort
    case FValueByte(_) => FType.FTypeByte
    case FValueFloat(_) => FType.FTypeFloat
    case FValueDouble(_) => FType.FTypeDouble
    case FValueReference(_, c) => FType.FTypeReference(c)
    case FValueChar(_) => FType.FTypeChar
    case FValueBoolean(_) => FType.FTypeBoolean
    case FValueArray(v) =>
      FType.FTypeArray(v.headOption.map(_.getType).getOrElse(FType.FTypeReference("java/lang/Object")))

  def toBytes: Array[Byte] = this match
    case FValueLong(v) => ByteBuffer.allocate(8).putLong(v).array()
    case FValueInt(v) => ByteBuffer.allocate(4).putInt(v).array()
    case FValueShort(v) => ByteBuffer.allocate(2).putShort(v).array()
    case FValueByte(v) => Array(v)
    case FValueFloat(v) => ByteBuffer.allocate(4).putFloat(v).array()
    case FValueDouble(v) => ByteBuffer.allocate(8).putDouble(v).array()
    case FValueReference(v, _) => ByteBuffer.allocate(4).putInt(v.getOrElse(0)).array()
    case FValueChar(v) => ByteBuffer.allocate(2).putChar(v).array()
    case FValueBoolean(v) => Array(if v then 1.toByte else 0.toByte)
    case FValueArray(v) => v.flatMap(_.toBytes)

  case FValueLong(value: Long)
  case FValueInt(value: Int)
  case FValueShort(value: Short)
  case FValueByte(value: Byte)
  case FValueFloat(value: Float)
  case FValueDouble(value: Double)
  case FValueReference(value: Option[Int], className: ClassName)
  case FValueChar(v: Char)
  case FValueBoolean(v: Boolean)
  case FValueArray[A <: FValue](v: Array[A])
