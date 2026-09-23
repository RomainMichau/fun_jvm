package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.FType.FTypeArray
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{Clazz, FType, InstanceField}

object WKString extends WellKnownClass {
  val className: String = "java/lang/String"

  def valueField(classLoader: FClassLoader): (Clazz, InstanceField) = {
    val clazz = getClazz(classLoader)
    val field = clazz.instanceFields(("value", FTypeArray.of(FType.FTypeChar)))
    (clazz, field)
  }

}
