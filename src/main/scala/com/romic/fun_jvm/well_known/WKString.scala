package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.Clazz.MethodDescriptor
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{Clazz, FType, InstanceField, JvmMethod}

object WKString extends WellKnownClass {
  val className: String = "java/lang/String"

  def valueField(classLoader: FClassLoader): (Clazz, InstanceField) = {
    val clazz = getClazz(classLoader)
    val field = clazz.instanceFields(("value", FType.FTypeArray(FType.FTypeChar)))
    (clazz, field)
  }

}
