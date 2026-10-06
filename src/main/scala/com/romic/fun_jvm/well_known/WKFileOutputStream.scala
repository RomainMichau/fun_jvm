package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{FThreadState, FType, FValue, InstanceClazz, MethodExecutorFactory}

object WKFileOutputStream extends WellKnownClass {
  val className: String = "java/io/FileOutputStream"

  def writeBytes(clazz: InstanceClazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val (bytesRef, offset, length, _) = NativeArgs.param4[FValue.FValueArrayRef, Int, Int, Boolean](params, threadState)
    val bytes = threadState.heap.getArray(FType.FTypeArray.of(FType.FTypeByte), bytesRef.toHeapAddr)
    if (offset < 0 || length < 0 || offset > bytes.length - length) {
      throw new IndexOutOfBoundsException(s"Invalid byte range [$offset, ${offset + length})")
    }
    System.out.write(bytes, offset, length)
    None
  }
}
