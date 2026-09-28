package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{InstanceClazz, FThreadState, FValue, MethodExecutorFactory}

object WKDouble extends WellKnownClass {
  val className: String = "java/lang/Double"

  def doubleToLongBits(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueLong] = {
    val dou = NativeArgs.param1[Double](params, s)
    Some(FValue.FValueLong(java.lang.Double.doubleToLongBits(dou)))
  }

  def doubleToRawLongBits(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueLong] = {
    val dou = NativeArgs.param1[Double](params, s)
    Some(FValue.FValueLong(java.lang.Double.doubleToRawLongBits(dou)))
  }

  def longBitsToDouble(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueDouble] = {
    val lon = NativeArgs.param1[Long](params, s)
    Some(FValue.FValueDouble(java.lang.Double.longBitsToDouble(lon)))
  }

}
