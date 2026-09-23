package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{Clazz, FThreadState, FValue, MethodExecutorFactory}

object WKClass extends WellKnownClass {
  val className: String = "java/lang/Class"

  def desiredAssertionStatus0(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = Some(FValue.FValueBoolean(true))

  def getPrimitiveClass(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] =
    params.head match {
      case ref: FValue.FValueClassRef =>
        val param = s.heap.readString(ref, s.classLoader)
        Some(s.classLoader.primitiveClass(param).toRef(className))
      case _ => throw new IllegalArgumentException(s"getPrimitiveClass expect a String in input")
    }

}
