package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.{MethodDescriptor, MethodId}
import com.romic.fun_jvm.FType.FTypeReference
import com.romic.fun_jvm.well_known.WKProperties

type NativeMethodRunWithClazz = (clazz: Clazz) => (FThreadState, MethodExecutorFactory, List[FValue]) => Unit
type NativeMethodRun = (FThreadState, MethodExecutorFactory, List[FValue]) => Unit

class NativeMethodCatalog(heap: Heap) {

  private def doNothing(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue]
  ): Unit = ()

  private def initProperties(clazz: Clazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue]
  ): Unit = {
    val (clazz, setPropertyMeth) = WKProperties.fun_property(threadState.classLoader)
    executorFactory.generateExecutorForMethod(setPropertyMeth, clazz, threadState).run()

  }

  private val nativeMethodCatalog: Map[MethodId, NativeMethodRunWithClazz] = Map(
    MethodId("java/lang/Object", "registerNatives", MethodDescriptor.void) -> doNothing,
    MethodId("java/lang/System", "registerNatives", MethodDescriptor.void) -> doNothing,
    MethodId(
      "java/lang/System",
      "initProperties",
      MethodDescriptor(List(FTypeReference("java/util/Properties")), FTypeReference("java/util/Properties"))
    ) ->
      initProperties
  )

  def get(clazz: Clazz, method: NativeMethod): NativeMethodRun = {
    print(method)
    nativeMethodCatalog(MethodId(clazz.name, method.methodName, method.methodDescriptor))(clazz)
  }

}
