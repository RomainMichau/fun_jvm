package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.*
import com.romic.fun_jvm.InstanceClazz.MethodDescriptor

object WKAccessController extends WellKnownClass {
  val className: String = "java/security/AccessController"

  def doPrivileged(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val actionRef = NativeArgs.param1[FValue.FValueClassRef](params, s)
    val actionClazz = actionRef.value match {
      case Some(v) => s.classLoader.getInstanceClass(v.className)
      case None => throw new NullPointerException("Cannot invoke doPrivileged on a null action")
    }
    val objectClazz = s.objectClazz match {
      case c: InstanceClazz => c
      case other => throw new RuntimeException(s"expected an instance class for Object, got $other")
    }
    val runDescriptor = MethodDescriptor(List.empty, FType.FTypeClassRef.of("java/lang/Object"))
    val (declaringClazz, method) = actionClazz.resolveVirtualMethod("run", runDescriptor, objectClazz)

    var captured: Option[FValue] = None
    val executor = executorFactory.generateExecutorForMethod(
      method,
      declaringClazz,
      s,
      (v: FValue) => captured = Some(v),
      List.empty,
      Some(actionRef)
    )
    executor.run() match {
      case BytecodeExecutor.FReturnValueOutcome(value) => value.orElse(captured)
      case BytecodeExecutor.FThrowableOutcome(ref, throwable) =>
        throw BytecodeExecutor.UncaughtFThrowable(ref, throwable)
    }
  }

  def getStackAccessControlContext(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = Some(FValue.FValueClassRef.null_)

}
