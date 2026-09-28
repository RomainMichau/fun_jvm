package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{FThreadState, FType, FValue, Frame, Heap, InstanceClazz, MethodExecutorFactory, zipWithIndex}

import scala.collection.mutable

object WKThrowable extends WellKnownClass {
  val className: String = "java/lang/Throwable"

  private def detailMessage(
    ref: FValue.FValueClassRef,
    throwableClazz: InstanceClazz,
    heap: Heap,
    classLoader: FClassLoader
  ): Option[String] = {
    val (owner, field) = throwableClazz.resolveField("detailMessage", FType.FTypeClassRef.of(WKString.className))
    heap.getField(owner, field, ref.toHeapAddr, classLoader) match {
      case r: FValue.FValueClassRef if !r.isNull => Some(heap.readString(r, classLoader))
      case _ => None
    }
  }

  def newWithMessage(className: String, message: String, s: FThreadState): (FValue.FValueClassRef, InstanceClazz) = {
    val excClazz = s.classLoader.getInstanceClass(className)
    val addr = s.heap.new_(excClazz)
    val ref = addr.toRef(excClazz.name)
    val (owner, field) = excClazz.resolveField("detailMessage", FType.FTypeClassRef.of(WKString.className))
    val msgRef = s.heap.storeStringLiteral(message, s.classLoader, s.objectClazz).toRef(WKString.className)
    s.heap.setField(owner, field, addr, msgRef, s.classLoader, s.objectClazz)
    (ref, excClazz)
  }

  def uncaughtMessage(
    ref: FValue.FValueClassRef,
    throwableClazz: InstanceClazz,
    heap: Heap,
    classLoader: FClassLoader
  ): String = {
    val dottedName = throwableClazz.name.replace('/', '.')
    val header = detailMessage(ref, throwableClazz, heap, classLoader) match {
      case Some(msg) => s"""[FJVM] Exception in thread "main" $dottedName: $msg"""
      case None => s"""[FJVM] Exception in thread "main" $dottedName"""
    }
    val trace = backtraceOf(ref.toHeapAddr)
      .map(f => s"\tat ${f.declaringClazz.name.replace('/', '.')} (pc ${f.pc})")
      .mkString("\n")
    if (trace.isEmpty) header else s"$header\n$trace"
  }

  // Real Throwable.backtrace is an opaque VM-internal blob, decoded later by the native
  // getStackTraceDepth()/getStackTraceElement(int). We control both ends here, so skip the
  // opaque encoding and just keep the captured frames Scala-side, keyed by the Throwable's
  // heap address.
  private val backtraces: mutable.Map[Heap.Address, List[Frame]] = mutable.Map.empty

  def backtraceOf(addr: Heap.Address): List[Frame] = backtraces.getOrElse(addr, List.empty)

  def fillInStackTrace(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val ref = NativeArgs.this_[FValue.FValueClassRef](this_, s)
    val frames = s.thread.stack.zipWithIndex.map(_._1).toList.drop(1)
    backtraces(ref.toHeapAddr) = frames
    Some(ref)
  }

}
