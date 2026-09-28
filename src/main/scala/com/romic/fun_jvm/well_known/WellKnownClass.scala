package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.InstanceClazz
import com.romic.fun_jvm.classloader.FClassLoader

trait WellKnownClass {
  def className: String
  def getClazz(classLoader: FClassLoader): InstanceClazz = classLoader.getInstanceClass(className)
}
