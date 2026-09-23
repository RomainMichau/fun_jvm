package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.ClassName

object FType {
  type Next = Int

  def parse(st: String): FType = parseOne(st, 0)._1

  // returns the parsed type, plus the index right after it in `st`
  def parseOne(st: String, i: Int): (FType, Next) = {
    st(i) match {
      case 'B' => (FType.FTypeByte, i + 1)
      case 'C' => (FType.FTypeChar, i + 1)
      case 'D' => (FType.FTypeDouble, i + 1)
      case 'F' => (FType.FTypeFloat, i + 1)
      case 'I' => (FType.FTypeInt, i + 1)
      case 'J' => (FType.FTypeLong, i + 1)
      case 'S' => (FType.FTypeShort, i + 1)
      case 'Z' => (FType.FTypeBoolean, i + 1)
      case 'L' =>
        val end = st.indexOf(';', i)
        val className = st.substring(i + 1, end)
        (FTypeClassRef.of(className), end + 1)
      case '[' =>
        val (elementType, nextI) = parseOne(st, i + 1)
        (FTypeArray.of(elementType), nextI)
    }
  }

  object FTypeArray {
    def of(type_ : FType): FType.FTypeArray = FType.FTypeArray(type_)

    // Every array is stored as a reference, regardless of its element type.
    val byteCount: Byte = 4
  }

  object FTypeClassRef {
    def of(className: ClassName): FType.FTypeClassRef = FType.FTypeClassRef(className)

    // Every object is stored as a reference, regardless of its class.
    val byteCount: Byte = 4
  }

  case object FTypeLong extends FType {
    def byteCount: Byte = 8
  }

  case object FTypeInt extends FType {
    def byteCount: Byte = 4
  }

  case object FTypeShort extends FType {
    def byteCount: Byte = 2
  }

  case object FTypeByte extends FType {
    def byteCount: Byte = 1
  }

  case object FTypeFloat extends FType {
    def byteCount: Byte = 4
  }

  case object FTypeDouble extends FType {
    def byteCount: Byte = 8
  }

  case class FTypeClassRef(className: String) extends FType {
    def byteCount: Byte = 4
  }

  case class FTypeArray(elementType: FType) extends FType {
    def byteCount: Byte = 4
  }

  case object FTypeChar extends FType {
    def byteCount: Byte = 2
  }

  case object Void extends FType {
    def byteCount: Byte = throw new RuntimeException("void has no byte count")
  }

  case object FTypeBoolean extends FType {
    def byteCount: Byte = 1
  }

}

sealed trait FType {
  def default: FValue = FValue.default(this)

  def byteCount: Byte

  override def toString: String = this match
    case FType.FTypeByte => "B"
    case FType.FTypeChar => "C"
    case FType.FTypeDouble => "D"
    case FType.FTypeFloat => "F"
    case FType.FTypeInt => "I"
    case FType.FTypeLong => "J"
    case FType.FTypeShort => "S"
    case FType.FTypeBoolean => "Z"
    case FType.Void => "V"
    case FType.FTypeClassRef(className) => s"L$className;"
    case FType.FTypeArray(elementType) => s"[$elementType"

}
