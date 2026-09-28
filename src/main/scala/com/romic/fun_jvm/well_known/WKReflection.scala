package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.*

object WKReflection extends WellKnownClass {
  val className: String = "sun/reflect/Reflection"

  def getCallerClass(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueClassRef] = {
    val callerClazz = s.thread.stack(1).declaringClazz
    val addr = callerClazz.getClassMirror(s.classLoader)
    Some(addr.toRef(WKClass.className))
  }

  def getClassAccessFlags(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val target = NativeArgs.param1[Clazz](params, s)
    val flags = target match {
      case c: InstanceClazz => c.accessFlags
      case _: ArrayClazz => 0x411
    }
    Some(FValue.FValueInt(flags))
  }

}
