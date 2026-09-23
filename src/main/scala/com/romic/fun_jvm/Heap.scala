package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.ClassName
import com.romic.fun_jvm.FType.{FTypeArray, FTypeChar}
import com.romic.fun_jvm.FValue.{FValueArrayRef, FValueClassRef}
import com.romic.fun_jvm.Heap.ArrayHeader
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.utils.Utils
import com.romic.fun_jvm.well_known.WKString

import java.nio.charset.StandardCharsets
import scala.collection.mutable

object Heap {

  object Address {
    def null_ : Address = 0

    def apply(int: Int): Address = int
  }

  extension (a: Address) {
    def toRef(className: ClassName): FValue.FValueClassRef = FValueClassRef.of(a, className)
    def toArrRef(innerType: FType): FValue.FValueArrayRef = FValueArrayRef.of(a, innerType)
    def toInt: Int = a
    def +(i: Int): Address = a + i
  }

  opaque type Address = Int

  private object ArrayHeader {
    val arrayHeaderSize: Int = 4
  }

  private case class ArrayHeader(size: Int, typeIndex: Int) {
    val toBytes: Array[Byte] = Utils.int2Bytes(size) ++ Utils.int2Bytes(typeIndex)
    val byteCount: Int = toBytes.length
  }

}

class Heap(size: Int) {
  private val heap: Array[Byte] = Array.fill[Byte](size)(0)
  // too optim, use an hashmap to avoir duplic
  private val arrayTypeIndex: mutable.ArrayBuffer[FType.FTypeArray] = mutable.ArrayBuffer.empty
  private var nextSlot: Int = 1
  private val literalPool: mutable.Map[String, Heap.Address] = mutable.Map.empty

  def toArrayForDebug: Array[Byte] = heap

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
        val chars = l.toCharArray
        val arrAddr = allocateArray(FType.FTypeArray.of(FTypeChar), chars.length)
        chars.zipWithIndex.foreach { case (c, i) =>
          writeElementInArr(arrAddr, i, FValue.FValueChar(c))
        }
        val ref = arrAddr.toArrRef(FTypeChar)
        val (stClazz, fieldRef) = WKString.valueField(classloader)
        val stRef = storeNew(stClazz)
        setField(stClazz, fieldRef, stRef, ref)
        literalPool(l) = stRef
        stRef

    }
  }

  def storeNew(clazz: Clazz): Heap.Address = {
    val clazzBytes: Array[Byte] = Utils.int2Bytes(
      clazz.classId
    ) ++ (clazz.instanceFields ++ clazz.secretInstanceField).toList.sortBy(_._2.fieldByteIndex).flatMap(f =>
      FValue.default(f._2.type_).toBytes
    ).toArray
    writeNew(clazzBytes)
  }

  def allocateArray(type_ : FType.FTypeArray, count: Int): Heap.Address = {
    val bytes: Array[Byte] =
      Array.fill[Array[Byte]](count * type_.elementType.byteCount)(type_.elementType.default.toBytes).flatten
    val typeIdx = arrayTypeIndex.length
    arrayTypeIndex += type_
    val header = ArrayHeader(count, typeIdx)
    writeNew(header.toBytes ++ bytes)
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

  private def writeBytes(ref: Heap.Address, bytes: Array[Byte]): Unit = {
    var c = ref.toInt
    bytes.foreach { b =>
      heap(c) = b
      c += 1
    }
  }

  private def readBytes(ref: Heap.Address, count: Int): Array[Byte] = {
    val start = ref.toInt
    heap.slice(start, start + count)
  }

  def writeElementInArr(arrRef: Heap.Address, index: Int, value: FValue): Unit = {
    val addr = arrRef + ArrayHeader.arrayHeaderSize + (index * value.getType.byteCount)
    writeBytes(addr, value.toBytes)
  }

  def readString(stringRef: FValue.FValueClassRef, classLoader: FClassLoader): String = {
    val (stClazz, valueField) = WKString.valueField(classLoader)
    val arrRef = getField(stClazz, valueField, stringRef.toHeapAddr) match {
      case arr: FValue.FValueArrayRef => arr
      case _ => throw new Exception(s"Field value of ${WKString.className} is expected to be an Array ref")
    }
    val array = getArray(FType.FTypeArray.of(FTypeChar), arrRef.toHeapAddr)
    String(array, StandardCharsets.UTF_16BE)
  }

  def getArrayChar(objRef: Heap.Address, idx: Int): Char = {
    val byteCount = FTypeChar.byteCount
    Utils.bytes2Char(readBytes(objRef + ArrayHeader.arrayHeaderSize + byteCount * idx, byteCount))
  }

  def getArrayRef(objRef: Heap.Address, idx: Int): Heap.Address = {
    val byteCount = FTypeArray.byteCount
    Heap.Address(Utils.bytes2Int(readBytes(objRef + ArrayHeader.arrayHeaderSize + byteCount * idx, byteCount)))
  }

  def getArray(array: FType.FTypeArray, objRef: Heap.Address): Array[Byte] = {
    val arrSize = getArrayLen(objRef)
    val byteCount = arrSize * array.elementType.byteCount
    readBytes(objRef + 4, byteCount)
  }

  private def getArrayHeader(objRef: Heap.Address): ArrayHeader = {
    val arrSize = getArrayLen(objRef)
    val arrTypeIdx = Utils.bytes2Int(readBytes(objRef + 4, 4))
    ArrayHeader(arrSize, arrTypeIdx)
  }

  def getArrayLen(objRef: Heap.Address): Int =
    Utils.bytes2Int(readBytes(objRef, 4))

  def getArrType(arrRef: Heap.Address): FType.FTypeArray =
    arrayTypeIndex(getArrayHeader(arrRef).typeIndex)

  def getField(clazz: Clazz, field: InstanceField, objRef: Heap.Address): FValue = {
    val addr = objRef + field.fieldByteIndex
    val bytes = readBytes(addr, field.type_.byteCount)
    FValue.fromBytes(field.type_, bytes)
  }

  def setField(clazz: Clazz, field: InstanceField, objRef: Heap.Address, value: FValue): Unit = {
    val isNullRef = value match {
      case FValue.FValueClassRef(None) | FValue.FValueArrayRef(None) => true
      case _ => false
    }
    if !isNullRef && value.getType != field.type_ then
      throw new RuntimeException(s"field type ${field.type_} is a different type than ${value.getType}")
    val addr = objRef + field.fieldByteIndex
    writeBytes(addr, value.toBytes)
  }

}
