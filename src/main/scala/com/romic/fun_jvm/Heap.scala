package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.{ClassName, InstanceFieldName}
import com.romic.fun_jvm.FValue.FValueReference
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.utils.Utils
import com.romic.fun_jvm.well_known.WKString

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.collection.mutable

object Heap {

  object Address {
    def apply(int: Int): Address = int
  }

  extension (a: Address) {
    def toRef(className: ClassName): FValue.FValueReference = FValue.FValueReference.of(a, className)
    def toArrRef(className: ClassName): FValue.FValueArray = FValue.FValueReference.of(a, className)
    def toInt: Int = a
  }

  opaque type Address = Int
}

class Heap(size: Int) {
  private val heap: Array[Byte] = Array.fill[Byte](size)(0)
  private var nextSlot: Int = 1
  private val literalPool: mutable.Map[String, Heap.Address] = mutable.Map.empty

  private def writeNew(bytes: Array[Byte]): Heap.Address = {
    val ref = nextSlot
    bytes.foreach { b =>
      heap(nextSlot) = b
      nextSlot += 1
    }
    Heap.Address(ref)
  }

  def storeStringLiteral(l: String, classloader: FClassLoader): Heap.Address = {
    literalPool.get(l) match {
      case Some(a) => a
      case None =>
        val bytes = l.getBytes(StandardCharsets.UTF_8)
        val a = writeNew(bytes)
        val (stClazz, fieldRef) = WKString.valueField(classloader)
        val stRef = storeNew(stClazz)
        setField(stClazz, fieldRef, stRef, a.toRef("[C"))
        literalPool(l) = stRef
        stRef

    }
  }

  def storeNew(clazz: Clazz): Heap.Address = {
    val clazzBytes: Array[Byte] = Utils.int2Bytes(
      clazz.classId
    ) ++ (clazz.instanceFields ++ clazz.secretInstanceField).toList.sortBy(_._2.fieldIndex).flatMap(f =>
      FValue.default(f._2.type_).toBytes
    ).toArray
    writeNew(clazzBytes)
  }

  def allocateArray(type_ : FType, count: Int): Heap.Address = {
    val bytes: Array[Byte] = Array.fill[Array[Byte]](count)(type_.default.toBytes).flatten
    val sizeBit = ByteBuffer.allocate(4).putInt(count).array()
    writeNew(sizeBit ++ bytes)
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

  def writeBytes(ref: Heap.Address, index: Int, bytes: Array[Byte]): Unit = {
    var c = ref.toInt + index
    bytes.foreach { b =>
      heap(c) = b
      c += 1
    }
  }

  def setField(clazz: Clazz, field: InstanceField, objRef: Heap.Address, value: FValue): Unit = {
    if value.getType != field.type_ then
      throw new RuntimeException(s"field type ${field.type_} is a different type than ${value.getType}")
    writeBytes(objRef, field.fieldIndex, value.toBytes)

  }

}
