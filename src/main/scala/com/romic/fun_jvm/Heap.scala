package com.romic.fun_jvm

import com.romic.fun_jvm.FValue.FValueReference

object Heap {
  object Address {
    def apply(int: Int): Address = int
  }
  opaque type Address = Int
}
class Heap(size: Int) {
  private val heap: Array[Byte] = Array.fill[Byte](size)(0)
  private var nextSlot: Int = 0


  def storeEmpty(clazz: Clazz): Heap.Address = {
    val clazzBytes: Array[Byte] = clazz.instanceFields.flatMap { f => FValue.initFromDescriptor(f._2).toBytes}.toArray
    val ref = nextSlot
    clazzBytes.foreach{b =>
      heap(nextSlot) = b
      nextSlot += 1
    }
    Heap.Address(ref)
  }

}