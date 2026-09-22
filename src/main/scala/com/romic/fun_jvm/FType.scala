package com.romic.fun_jvm

object FType {
  type Next = Int

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
        (FType.FTypeReference(className), end + 1)
      case '[' =>
        val (elementType, nextI) = parseOne(st, i + 1)
        (FType.FTypeArray(elementType), nextI)
    }
  }

}

enum FType:
  def default: FValue = FValue.default(this)

  case FTypeLong
  case FTypeInt
  case FTypeShort
  case FTypeByte
  case FTypeFloat
  case FTypeDouble
  case FTypeReference(className: String)
  case FTypeArray(elementType: FType)
  case FTypeChar
  case Void
  case FTypeBoolean
