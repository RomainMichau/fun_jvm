package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.{MethodDescriptor, MethodId}

type NativeMethodRun = () => Unit

object NativeMethodCatalog {
  private def doNothing(): Unit = ()

  private val nativeMethodCatalog: Map[MethodId, NativeMethodRun] = Map(
    MethodId("java/lang/Object", "registerNatives", MethodDescriptor.void) -> doNothing,
    MethodId("java/lang/System", "registerNatives", MethodDescriptor.void) -> doNothing
  )
  
  def get(clazz: Clazz, method: NativeMethod): NativeMethodRun = nativeMethodCatalog(MethodId(clazz.name, method.methodName, method.methodDescriptor))

}


