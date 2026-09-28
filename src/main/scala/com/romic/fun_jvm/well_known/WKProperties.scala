package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.InstanceClazz.MethodDescriptor
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{InstanceClazz, JvmMethod}

object WKProperties extends WellKnownClass {
  val fun_nat_st_initProperties = "initProperties"
  val fun_setProperty = "setProperty"

  val className: String = "java/util/Properties"

  def fun_setProperty(classLoader: FClassLoader): (InstanceClazz, JvmMethod) = {
    val clazz = getClazz(classLoader)
    val func = clazz.jvmMethods((
      "setProperty",
      MethodDescriptor.parseMethodDescriptor("(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;")
    ))
    (clazz, func)
  }

}
