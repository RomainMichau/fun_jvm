package com.romic.fun_jvm

import FValue.FValueInt
import com.romic.fun_jvm.Clazz.ClassName
import com.romic.fun_jvm.utils.Utils

object FValue {

  extension (i: FValueInt) {
    def +(i2: FValueInt): FValueInt = FValueInt(i.value + i2.value)
  }

  extension (i: FValueClassRef) {
    def toHeapAddr: Heap.Address = i.value.map(_.addr).getOrElse(Heap.Address.null_)
  }

  extension (i: FValueArrayRef) {
    def toHeapAddr: Heap.Address = i.value.map(_.addr).getOrElse(Heap.Address.null_)
  }

  object FValueClassRef {
    def null_ : FValueClassRef = FValueClassRef(None)

    def of(addr: Heap.Address, className: ClassName): FValueClassRef =
      FValueClassRef(Some(ClassReferenceVal(addr, className)))

  }

  object FValueArrayRef {
    def null_ : FValueArrayRef = FValueArrayRef(None)

    def of(addr: Heap.Address, innerType: FType): FValueArrayRef =
      FValueArrayRef(Some(ArrReferenceVal(addr, innerType)))

  }

  def fromBytes(type_ : FType, bytes: Array[Byte]): FValue = type_ match {
    case FType.FTypeLong => FValueLong(Utils.bytes2Long(bytes))
    case FType.FTypeInt => FValueInt(Utils.bytes2Int(bytes))
    case FType.FTypeShort => FValueShort(Utils.bytes2Short(bytes))
    case FType.FTypeByte => FValueByte(Utils.bytes2Byte(bytes))
    case FType.FTypeFloat => FValueFloat(Utils.bytes2Float(bytes))
    case FType.FTypeDouble => FValueDouble(Utils.bytes2Double(bytes))
    case FType.FTypeChar => FValueChar(Utils.bytes2Char(bytes))
    case FType.FTypeBoolean => FValueBoolean(Utils.bytes2Boolean(bytes))
    case FType.FTypeClassRef(className) =>
      Utils.bytes2Int(bytes) match {
        case 0 => FValueClassRef.null_
        case addr => FValueClassRef.of(Heap.Address(addr), className)
      }
    case FType.FTypeArray(elementType) =>
      Utils.bytes2Int(bytes) match {
        case 0 => FValueArrayRef.null_
        case addr => FValueArrayRef.of(Heap.Address(addr), elementType)
      }
    case FType.Void => throw new RuntimeException("void has no value")
  }

  // JVM computational type "int" also covers boolean/byte/short/char on the operand stack
  // and in local variables — real bytecode freely mixes them with true ints (e.g. a native
  // method returning boolean feeding directly into ifne). Normalize them here instead of
  // treating them as distinct stack types.
  def asInt(v: FValue): FValueInt = v match {
    case i: FValueInt => i
    case FValueBoolean(b) => FValueInt(if (b) 1 else 0)
    case FValueByte(b) => FValueInt(b.toInt)
    case FValueShort(s) => FValueInt(s.toInt)
    case FValueChar(c) => FValueInt(c.toInt)
    case v => throw new RuntimeException(s"expected an int-like value, got $v")
  }

  def default(v: FType): FValue = v match {
    case FType.FTypeLong => FValueLong(0)
    case FType.FTypeInt => FValueInt(0)
    case FType.FTypeShort => FValueShort(0)
    case FType.FTypeByte => FValueByte(0)
    case FType.FTypeFloat => FValueFloat(0)
    case FType.FTypeDouble => FValueDouble(0)
    case FType.FTypeClassRef(_) => FValueClassRef.null_
    case FType.FTypeArray(_) => FValueArrayRef.null_
    case FType.FTypeChar => FValueChar('\u0000')
    case FType.FTypeBoolean => FValueBoolean(false)
    case FType.Void => throw new RuntimeException("void has no default value")
  }

  case class FValueLong(value: Long) extends FValue
  case class FValueInt(value: Int) extends FValue
  case class FValueShort(value: Short) extends FValue
  case class FValueByte(value: Byte) extends FValue
  case class FValueFloat(value: Float) extends FValue
  case class FValueDouble(value: Double) extends FValue
  case class FValueChar(v: Char) extends FValue
  case class FValueBoolean(v: Boolean) extends FValue
  case class FValueClassRef(value: Option[ClassReferenceVal]) extends FValueRef
  case class FValueArrayRef(value: Option[ArrReferenceVal]) extends FValueRef

}

case class ClassReferenceVal(addr: Heap.Address, className: ClassName)

case class ArrReferenceVal(addr: Heap.Address, innertType: FType)

sealed trait FValueRef extends FValue

sealed trait FValue {

  def getType: FType = this match
    case FValue.FValueLong(_) => FType.FTypeLong
    case FValue.FValueInt(_) => FType.FTypeInt
    case FValue.FValueShort(_) => FType.FTypeShort
    case FValue.FValueByte(_) => FType.FTypeByte
    case FValue.FValueFloat(_) => FType.FTypeFloat
    case FValue.FValueDouble(_) => FType.FTypeDouble
    case FValue.FValueChar(_) => FType.FTypeChar
    case FValue.FValueBoolean(_) => FType.FTypeBoolean
    case FValue.FValueClassRef(Some(v)) =>
      FType.FTypeClassRef(v.className)
    case FValue.FValueClassRef(None) =>
      throw new NullPointerException(s"Cannot get type of null value")
    case FValue.FValueArrayRef(Some(v)) => FType.FTypeArray(v.innertType)
    case FValue.FValueArrayRef(None) => throw new NullPointerException(s"Cannot get type of null value")

  def toBytes: Array[Byte] = this match
    case FValue.FValueLong(v) => Utils.long2Bytes(v)
    case FValue.FValueInt(v) => Utils.int2Bytes(v)
    case FValue.FValueShort(v) => Utils.short2Bytes(v)
    case FValue.FValueByte(v) => Utils.byte2Bytes(v)
    case FValue.FValueFloat(v) => Utils.float2Bytes(v)
    case FValue.FValueDouble(v) => Utils.double2Bytes(v)
    case FValue.FValueChar(v) => Utils.char2Bytes(v)
    case FValue.FValueBoolean(v) => Utils.boolean2Bytes(v)
    case FValue.FValueClassRef(v) => Utils.int2Bytes(v.map(_.addr.toInt).getOrElse(0))
    case FValue.FValueArrayRef(v) => Utils.int2Bytes(v.map(_.addr.toInt).getOrElse(0))

}
