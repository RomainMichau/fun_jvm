package com.romic.fun_jvm

import com.romic.fun_jvm.FValue.FValueReference
import com.romic.fun_jvm.utils.Utils

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.collection.mutable

object Heap {
  object Address {
    def apply(int: Int): Address = int
  }

  extension (a: Address) {
    def toRef: FValue.FValueReference = FValue.FValueReference.of(a)
    def toInt: Int = a
  }

  opaque type Address = Int
}

class Heap(size: Int) { 
  private val heap: Array[Byte] = Array.fill[Byte](size)(0)
  private var nextSlot: Int = 1
  private val literalPool: mutable.Map[String, Heap.Address] = mutable.Map.empty


  private def writeOnHeap(bytes: Array[Byte]): Heap.Address = {
    val ref = nextSlot
    bytes.foreach { b =>
      heap(nextSlot) = b
      nextSlot += 1
    }
    Heap.Address(ref)
  }

  def storeStringLiteral(l: String): Heap.Address = {
    literalPool.get(l) match {
      case Some(a) => a
      case None =>
        val bytes = l.getBytes(StandardCharsets.UTF_8)
        val a = writeOnHeap(bytes)
        literalPool(l) = a
        a

    }
  }

  def storeNew(clazz: Clazz): Heap.Address = {
    val clazzBytes: Array[Byte] = clazz.instanceFields.flatMap { f => FValue.initFromDescriptor(f._2).toBytes }.toArray
    writeOnHeap(clazzBytes)
  }

  def allocateArray(type_ : FType, count: Int): Heap.Address = {
    val bytes: Array[Byte] = Array.fill[Array[Byte]](count)(type_.default.toBytes).flatten
    val sizeBit = ByteBuffer.allocate(4).putInt(count).array()
    writeOnHeap(sizeBit ++ bytes)
  }

  def debug: String = {
    val used = heap.slice(0, nextSlot).map(b => f"$b%02x").mkString(" ")
    s"""Heap(
       |size: $size
       |used: $nextSlot
       |bytes: $used
       |literalPool: ${literalPool.mkString(", ")}
       |)""".stripMargin
  }
  
  def writeBytes2Arr(arrayRef: Heap.Address, index: Int, bytes: Array[Byte]): Unit = {
    var c = arrayRef.toInt + index
    bytes.foreach{b =>
      heap(c) = b
      c += 1
    }
  }

}