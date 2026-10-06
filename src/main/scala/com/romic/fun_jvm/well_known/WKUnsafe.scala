package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.utils.Utils
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

  private var nextMemoryAddress: Long = 1L
  private var nativeMemory: Map[Long, Array[Byte]] = Map.empty

  private def memoryAt(address: Long, byteCount: Int): (Array[Byte], Int) =
    nativeMemory.collectFirst {
      case (base, memory) if address >= base && address <= base + memory.length - byteCount =>
        (memory, (address - base).toInt)
    }.getOrElse(throw new IllegalArgumentException(s"Invalid native memory range at $address for $byteCount bytes"))

  def allocateMemory(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val byteCount = NativeArgs.param1[Long](params, s)
    if (byteCount < 0 || byteCount > Int.MaxValue) {
      throw new IllegalArgumentException(s"Unsupported native memory size $byteCount")
    }
    val address = nextMemoryAddress
    nativeMemory += address -> Array.fill(byteCount.toInt)(0.toByte)
    nextMemoryAddress += byteCount + 1
    Some(FValue.FValueLong(address))
  }

  def putLong(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val (address, value) = NativeArgs.param2[Long, Long](params, s)
    val (memory, offset) = memoryAt(address, java.lang.Long.BYTES)
    Array.copy(Utils.long2Bytes(value), 0, memory, offset, java.lang.Long.BYTES)
    None
  }

  def getByte(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val address = NativeArgs.param1[Long](params, s)
    val (memory, offset) = memoryAt(address, 1)
    Some(FValue.FValueByte(memory(offset)))
  }

  def freeMemory(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val address = NativeArgs.param1[Long](params, s)
    if (!nativeMemory.contains(address)) {
      throw new IllegalArgumentException(s"Unknown native memory address $address")
    }
    nativeMemory -= address
    None
  }

}
