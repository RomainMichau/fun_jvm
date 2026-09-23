package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{Clazz, FThreadState, FValue, MethodExecutorFactory}

object WKFloat extends WellKnownClass {
  val className: String = "java/lang/Float"

  def floatToRawIntBits(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueInt] = params.head match {
    case FValue.FValueFloat(fl) =>
      Some(FValue.FValueInt(java.lang.Float.floatToRawIntBits(fl)))
    case _ => throw new IllegalArgumentException(s"floatToRawIntBits expect a float in input")
  }

}
