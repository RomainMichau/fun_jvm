package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.InstanceClazz.MethodDescriptor
import com.romic.fun_jvm.{BytecodeExecutor, FThreadState, FType, FValue, Heap, InstanceClazz, MethodExecutorFactory}

object WKThread extends WellKnownClass {
  val className: String = "java/lang/Thread"

  private var mainThread: Option[Heap.Address] = None

  def currentThread(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueClassRef] = {
    val addr = mainThread match {
      case Some(a) =>
        a
      case None =>
        val threadClazz = getClazz(s.classLoader)
        val a = s.heap.new_(threadClazz)
        mainThread = Some(a)
        val (owner, priorityField) = threadClazz.resolveField("priority", FType.FTypeInt)
        s.heap.setField(owner, priorityField, a, FValue.FValueInt(5), s.classLoader, s.objectClazz)
        initMainThread(a, s, executorFactory)
        a
    }
    Some(addr.toRef(className))
  }

  def isAlive(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val ref = NativeArgs.this_[FValue.FValueClassRef](this_, s)
    val threadClazz = getClazz(s.classLoader)
    val (owner, statusField) = threadClazz.resolveField("threadStatus", FType.FTypeInt)
    val status = s.heap.getField(owner, statusField, ref.toHeapAddr, s.classLoader) match {
      case FValue.FValueInt(v) => v
      case other => throw new RuntimeException(s"expected threadStatus to be an int, got $other")
    }
    Some(FValue.FValueBoolean(status != 0))
  }

  private def initMainThread(addr: Heap.Address, s: FThreadState, executorFactory: MethodExecutorFactory): Unit = {
    val groupRef = WKThreadGroup.newRootGroup(s, executorFactory)

    val threadClazz = getClazz(s.classLoader)
    val nameRef = s.heap.storeStringLiteral("main", s.classLoader, s.objectClazz).toRef(WKString.className)
    val accClazz = s.classLoader.getInstanceClass("java/security/AccessControlContext")
    val accRef = s.heap.new_(accClazz).toRef(accClazz.name)
    val initMethod = threadClazz.jvmMethods(
      (
        "init",
        MethodDescriptor.parseMethodDescriptor(
          "(Ljava/lang/ThreadGroup;Ljava/lang/Runnable;Ljava/lang/String;JLjava/security/AccessControlContext;Z)V"
        )
      )
    )
    val initParams =
      List(groupRef, FValue.FValueClassRef.null_, nameRef, FValue.FValueLong(0), accRef, FValue.FValueBoolean(true))
    executorFactory
      .generateExecutorForMethod(
        initMethod,
        threadClazz,
        s,
        BytecodeExecutor.sinkReturn,
        initParams,
        Some(addr.toRef(className))
      )
      .run()
  }

}

object WKThreadGroup extends WellKnownClass {
  val className: String = "java/lang/ThreadGroup"

  def newRootGroup(s: FThreadState, executorFactory: MethodExecutorFactory): FValue.FValueClassRef = {
    val clazz = getClazz(s.classLoader)
    val groupRef = s.heap.new_(clazz).toRef(clazz.name)
    val init = clazz.jvmMethods(("<init>", MethodDescriptor.parseMethodDescriptor("()V")))
    executorFactory
      .generateExecutorForMethod(init, clazz, s, BytecodeExecutor.sinkReturn, List.empty, Some(groupRef))
      .run()
    groupRef
  }

}
