package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.{MethodDescriptor, MethodId}
import com.romic.fun_jvm.FType.FTypeReference
import com.romic.fun_jvm.well_known.WKProperties

type NativeMethodRunWithClazz = (clazz: Clazz) => (FThreadState, MethodExecutorFactory, List[FValue]) => Option[FValue]
type NativeMethodRun = (FThreadState, MethodExecutorFactory, List[FValue]) => Option[FValue]

class NativeMethodCatalog(heap: Heap) {

  private def doNothing(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue]
  ): Option[FValue] = None

  private def initProperties(clazz: Clazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue]
  ): Option[FValue] = {
    ???
    val (clazz, setPropertyMeth) = WKProperties.fun_property(threadState.classLoader)
    executorFactory.generateExecutorForMethod(setPropertyMeth, clazz, threadState).run()
    None
  }

  private def getPrimitiveClass(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue]
  ): Option[FValue] =
    ???

  private val nativeMethodCatalog: Map[MethodId, NativeMethodRunWithClazz] = Map(
    MethodId("java/lang/Object", "registerNatives", MethodDescriptor.void) -> doNothing,
    MethodId("java/lang/System", "registerNatives", MethodDescriptor.void) -> doNothing,
    MethodId("java/lang/Class", "registerNatives", MethodDescriptor.void) -> doNothing,
    // TODO: stub for now, should allocate/cache a Class mirror per primitive name via
    // heap.storeNew(ClassClazz.getClassClazz(heap)) + setField("name", ...) and return it
    MethodId(
      "java/lang/Class",
      "getPrimitiveClass",
      MethodDescriptor(List(FTypeReference("java/lang/String")), FTypeReference("java/lang/Class"))
    ) -> getPrimitiveClass,
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
