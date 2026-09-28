package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{InstanceClazz, FThreadState, FValue, MethodExecutorFactory}

object WKFloat extends WellKnownClass {
  val className: String = "java/lang/Float"

  def floatToRawIntBits(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueInt] = {
    val fl = NativeArgs.param1[Float](params, s)
    Some(FValue.FValueInt(java.lang.Float.floatToRawIntBits(fl)))
  }

}
