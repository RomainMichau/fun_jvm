package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.utils.Utils.toFValue
import com.romic.fun_jvm.{
  ArrayClazz,
  Clazz,
  ClassReferenceVal,
  FThreadState,
  FValue,
  Heap,
  InstanceClazz,
  MethodExecutorFactory
}

object WKUnsafe extends WellKnownClass {
  val className: String = "sun/misc/Unsafe"

  def arrayBaseOffset(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueInt] =
    Some(Heap.ArrayHeader.arrayHeaderByteCount.toFValue)

  def arrayIndexScale(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueInt] =
    NativeArgs.param1[Clazz](params, s) match {
      case arrayClazz: ArrayClazz => Some(arrayClazz.innerType.byteCount.toInt.toFValue)
      case other => throw new IllegalArgumentException(s"arrayIndexScale expects an array class, got $other")
    }

  def addressSize(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue.FValueInt] =
    Some(Heap.addressByteCount.toFValue)

  def compareAndSwapObject(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val (o, offset, expected, update) =
      NativeArgs.param4[FValue.FValueClassRef, Long, FValue.FValueClassRef, FValue.FValueClassRef](params, s)
    val current = s.heap.getRefAt(o.toHeapAddr, offset)
    if (current == expected.toHeapAddr) {
      s.heap.putRefAt(o.toHeapAddr, offset, update.toHeapAddr)
      Some(FValue.FValueBoolean(true))
    } else {
      Some(FValue.FValueBoolean(false))
    }
  }

  def objectFieldOffset(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val fieldRef = NativeArgs.param1[FValue.FValueClassRef](params, s)
    val (_, field) = WKField.resolveDeclaredField(s, fieldRef)
    Some(FValue.FValueLong(field.fieldByteIndex.toLong))
  }

  def getIntVolatile(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val (o, offset) = NativeArgs.param2[FValue.FValueClassRef, Long](params, s)
    Some(FValue.FValueInt(s.heap.getIntAt(o.toHeapAddr, offset)))
  }

  def compareAndSwapInt(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val (o, offset, expected, update) = NativeArgs.param4[FValue.FValueClassRef, Long, Int, Int](params, s)
    val current = s.heap.getIntAt(o.toHeapAddr, offset)
    if (current == expected) {
      s.heap.putIntAt(o.toHeapAddr, offset, update)
      Some(FValue.FValueBoolean(true))
    } else {
      Some(FValue.FValueBoolean(false))
    }
  }

}
