package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.utils.Utils.toFValue
import com.romic.fun_jvm.{FThreadState, FValue, InstanceClazz, MethodExecutorFactory}

object WKObject extends WellKnownClass {
  val className: String = "java/lang/Object"

  def hashCode_(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueInt] =
    Some(FValue.FValueInt(NativeArgs.this_[FValue.FValueClassRef](this_, s).toHeapAddr.toInt))

  def getClass_(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val ref = NativeArgs.this_[FValue.FValueClassRef](this_, s)
    val underlyingClazz = s.heap.getObjectClazz(ref.toHeapAddr, s.classLoader)
    Some(underlyingClazz.getClassMirror(s.classLoader).toRef(WKClass.className))
  }

}
