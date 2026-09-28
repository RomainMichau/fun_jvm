package com.romic.fun_jvm

import FValue.FValueInt
import com.romic.fun_jvm.InstanceClazz.ClassName
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

  // Reverse of asInt: narrows a stack/local int back down to the declared int-like field type
  // (boolean/byte/short/char) so it's written with that type's byte width instead of an int's.
  def narrowInt(type_ : FType, i: FValueInt): FValue = type_ match {
    case FType.FTypeInt => i
    case FType.FTypeBoolean => FValueBoolean(i.value != 0)
    case FType.FTypeByte => FValueByte(i.value.toByte)
    case FType.FTypeShort => FValueShort(i.value.toShort)
    case FType.FTypeChar => FValueChar(i.value.toChar)
    case t => throw new RuntimeException(s"expected an int-like field type, got $t")
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

  case class FValueLong(value: Long) extends FValue {
    def getType: FType.FTypeLong.type = FType.FTypeLong
  }

  case class FValueInt(value: Int) extends FValue {
    def getType: FType.FTypeInt.type = FType.FTypeInt
  }

  case class FValueShort(value: Short) extends FValue {
    def getType: FType.FTypeShort.type = FType.FTypeShort
  }

  case class FValueByte(value: Byte) extends FValue {
    def getType: FType.FTypeByte.type = FType.FTypeByte
  }

  case class FValueFloat(value: Float) extends FValue {
    def getType: FType.FTypeFloat.type = FType.FTypeFloat
  }

  case class FValueDouble(value: Double) extends FValue {
    def getType: FType.FTypeDouble.type = FType.FTypeDouble
  }

  case class FValueChar(v: Char) extends FValue {
    def getType: FType.FTypeChar.type = FType.FTypeChar
  }

  case class FValueBoolean(v: Boolean) extends FValue {
    def getType: FType.FTypeBoolean.type = FType.FTypeBoolean
  }

  case class FValueClassRef(value: Option[ClassReferenceVal]) extends FValueRef {
    def addr: Option[Heap.Address] = value.map(_.addr)

    def isNull: Boolean = value.isEmpty

    def getType: FType.FTypeClassRef = value match {
      case Some(v) => FType.FTypeClassRef(v.className)
      case None => throw new NullPointerException(s"Cannot get type of null value")
    }

  }

  case class FValueArrayRef(value: Option[ArrReferenceVal]) extends FValueRef {
    def addr: Option[Heap.Address] = value.map(_.addr)

    def isNull: Boolean = value.isEmpty

    def getType: FType.FTypeArray = value match {
      case Some(v) => FType.FTypeArray(v.innertType)
      case None => throw new NullPointerException(s"Cannot get type of null value")
    }

  }

}

case class ClassReferenceVal(addr: Heap.Address, className: ClassName)

case class ArrReferenceVal(addr: Heap.Address, innertType: FType) {
  def className: ClassName = s"[$innertType"
}

sealed trait FValueRef extends FValue {
  def isNull: Boolean
  def addr: Option[Heap.Address]
  def getType: FTypeRef
}

sealed trait FValue {

  def getType: FType

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
