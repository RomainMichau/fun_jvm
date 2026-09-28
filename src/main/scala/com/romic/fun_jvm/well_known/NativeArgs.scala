package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{Clazz, FThreadState, FValue}

trait FValueDecoder[A] {
  def decode(v: FValue, s: FThreadState): A
}

object FValueDecoder {
  given FValueDecoder[Int] = (v, _) => FValue.asInt(v).value

  given FValueDecoder[Long] = (v, _) =>
    v match {
      case FValue.FValueLong(l) => l
      case other => throw new IllegalArgumentException(s"expected a long, got $other")
    }

  given FValueDecoder[Float] = (v, _) =>
    v match {
      case FValue.FValueFloat(f) => f
      case other => throw new IllegalArgumentException(s"expected a float, got $other")
    }

  given FValueDecoder[Double] = (v, _) =>
    v match {
      case FValue.FValueDouble(d) => d
      case other => throw new IllegalArgumentException(s"expected a double, got $other")
    }

  given FValueDecoder[Boolean] = (v, _) => FValue.asInt(v).value != 0

  given FValueDecoder[Byte] = (v, _) => FValue.asInt(v).value.toByte

  given FValueDecoder[Short] = (v, _) => FValue.asInt(v).value.toShort

  given FValueDecoder[Char] = (v, _) => FValue.asInt(v).value.toChar

  given FValueDecoder[FValue.FValueClassRef] = (v, _) =>
    v match {
      case r: FValue.FValueClassRef => r
      case other => throw new IllegalArgumentException(s"expected a class ref, got $other")
    }

  given FValueDecoder[String] = (v, s) =>
    v match {
      case r: FValue.FValueClassRef => s.heap.readString(r, s.classLoader)
      case other => throw new IllegalArgumentException(s"expected a String ref, got $other")
    }

  // This is to be used only on reference to java/lang/Class
  given FValueDecoder[Clazz] = (v, s) =>
    v match {
      case r: FValue.FValueClassRef => WKClass.getUnderlyingClazz(r, s)
      case other => throw new IllegalArgumentException(s"expected a Class ref, got $other")
    }

  given FValueDecoder[Unit] = (v, s) => ()
}

object NativeArgs {

  def param1[A](params: List[FValue], s: FThreadState)(using da: FValueDecoder[A]): A =
    params match {
      case a :: _ => da.decode(a, s)
      case _ => throw new IllegalArgumentException(s"expected 1 param, got $params")
    }

  def param2[A, B](params: List[FValue], s: FThreadState)(using da: FValueDecoder[A], db: FValueDecoder[B]): (A, B) =
    params match {
      case a :: b :: _ => (da.decode(a, s), db.decode(b, s))
      case _ => throw new IllegalArgumentException(s"expected 2 params, got $params")
    }

  def param3[A, B, C](
    params: List[FValue],
    s: FThreadState
  )(using da: FValueDecoder[A], db: FValueDecoder[B], dc: FValueDecoder[C]): (A, B, C) =
    params match {
      case a :: b :: c :: _ => (da.decode(a, s), db.decode(b, s), dc.decode(c, s))
      case _ => throw new IllegalArgumentException(s"expected 3 params, got $params")
    }

  def param4[A, B, C, D](
    params: List[FValue],
    s: FThreadState
  )(using da: FValueDecoder[A], db: FValueDecoder[B], dc: FValueDecoder[C], dd: FValueDecoder[D]): (A, B, C, D) =
    params match {
      case a :: b :: c :: d :: _ => (da.decode(a, s), db.decode(b, s), dc.decode(c, s), dd.decode(d, s))
      case _ => throw new IllegalArgumentException(s"expected 4 params, got $params")
    }

  def this_[A](this_ : Option[FValue.FValueClassRef], s: FThreadState)(using da: FValueDecoder[A]): A =
    this_ match {
      case Some(v) => da.decode(v, s)
      case None => throw new IllegalArgumentException("Missing this pointer")
    }

}
