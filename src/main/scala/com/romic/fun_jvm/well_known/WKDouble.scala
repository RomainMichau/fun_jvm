package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{Clazz, FThreadState, FValue, MethodExecutorFactory}

object WKDouble extends WellKnownClass {
  val className: String = "java/lang/Double"

  def doubleToLongBits(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueLong] = params.head match {
    case FValue.FValueDouble(dou) =>
      Some(FValue.FValueLong(java.lang.Double.doubleToLongBits(dou)))
    case _ => throw new IllegalArgumentException(s"doubleToLongBits expect a double in input")
  }

  def doubleToRawLongBits(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueLong] = params.head match {
    case FValue.FValueDouble(dou) =>
      Some(FValue.FValueLong(java.lang.Double.doubleToRawLongBits(dou)))
    case _ => throw new IllegalArgumentException(s"doubleToLongBits expect a double in input")
  }

  def longBitsToDouble(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueDouble] = params.head match {
    case FValue.FValueLong(lon) =>
      Some(FValue.FValueDouble(java.lang.Double.longBitsToDouble(lon)))
    case x => throw new IllegalArgumentException(s"longBitsToDouble long a double in input, got $x")
  }

}
