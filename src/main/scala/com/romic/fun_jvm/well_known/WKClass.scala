package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.Clazz.MethodDescriptor
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{Clazz, JvmMethod}

object WKClass extends WellKnownClass {
  val className: String = "java/lang/Class"

}
