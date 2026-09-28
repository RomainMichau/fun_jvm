package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.FType.FTypeArray
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{FThreadState, FType, FValue, InstanceClazz, InstanceField, MethodExecutorFactory}

object WKString extends WellKnownClass {
  val className: String = "java/lang/String"

  def valueField(classLoader: FClassLoader): (InstanceClazz, InstanceField) = {
    val clazz = getClazz(classLoader)
    val field = clazz.instancesField(("value", FTypeArray.of(FType.FTypeChar)))
    (clazz, field)
  }

  def intern(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val _this = NativeArgs.this_[FValue.FValueClassRef](this_, s)
    val (thisClazz, valueField_) = valueField(s.classLoader)
    val str = s.heap.readString(_this, s.classLoader)
    Some(s.heap.storeStringLiteral(str, s.classLoader, s.objectClazz).toRef(this.className))
  }

}
