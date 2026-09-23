package com.romic.fun_jvm

import com.romic.fun_jvm.FValue.FValueClassRef
import com.romic.fun_jvm.well_known.{WKClass, WKDouble, WKFloat, WKSystem}

type NativeMethodRunWithClazz =
  (clazz: Clazz) => (FThreadState, MethodExecutorFactory, List[FValue], Option[FValueClassRef]) => Option[FValue]

type NativeMethodRun = (FThreadState, MethodExecutorFactory, List[FValue], Option[FValueClassRef]) => Option[FValue]

class NativeMethodCatalog(heap: Heap) {

  // Applies to registerNatives regardless of which class declares it, so it can't live in a
  // single WK<Class> file the way the other native methods below do.
  private def doNothing(clazz: Clazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = None

  def get(clazz: Clazz, method: NativeMethod): NativeMethodRun = {
    println(s"Preparing method ${clazz.name} $method")
    val run: NativeMethodRunWithClazz = (clazz.name, method.methodName, method.methodDescriptor.toString) match {
      case (_, "registerNatives", _) => doNothing
      case ("java/lang/Class", "desiredAssertionStatus0", "(Ljava/lang/Class;)Z") => WKClass.desiredAssertionStatus0
      // TODO: stub for now, should allocate/cache a Class mirror per primitive name via
      // heap.storeNew(ClassClazz.getClassClazz(heap)) + setField("name", ...) and return it
      case ("java/lang/Class", "getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;") =>
        WKClass.getPrimitiveClass
      case ("java/lang/System", "initProperties", "(Ljava/util/Properties;)Ljava/util/Properties;") =>
        WKSystem.initProperties
      case ("java/lang/Float", "floatToRawIntBits", "(F)I") => WKFloat.floatToRawIntBits
      case ("java/lang/Double", "doubleToLongBits", "(D)J") => WKDouble.doubleToLongBits
      case ("java/lang/Double", "doubleToRawLongBits", "(D)J") => WKDouble.doubleToRawLongBits
      case ("java/lang/Double", "longBitsToDouble", "(J)D") => WKDouble.longBitsToDouble
      case ("sun/misc/VM", "initialize", "()V") => doNothing
      case other => throw new NoSuchElementException(s"No native method registered for $other")
    }
    run(clazz)
  }

}
