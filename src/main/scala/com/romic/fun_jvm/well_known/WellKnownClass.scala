package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.Clazz
import com.romic.fun_jvm.classloader.FClassLoader

trait WellKnownClass {
  def className: String
  def getClazz(classLoader: FClassLoader): Clazz = classLoader.getClass(className)
}
