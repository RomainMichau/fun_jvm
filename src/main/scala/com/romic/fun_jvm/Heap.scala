package com.romic.fun_jvm

import com.romic.fun_jvm
import com.romic.fun_jvm.InstanceClazz.ClassName
import com.romic.fun_jvm.FType.{FTypeArray, FTypeChar}
import com.romic.fun_jvm.FValue.{FValueArrayRef, FValueClassRef}
import com.romic.fun_jvm.Heap.{ArrayHeader, InstanceClassHeader}
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.classloader.FClassLoader.ClassId
import com.romic.fun_jvm.utils.{Assignability, Utils}
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

  val addressByteCount: Int = 4

  object ArrayHeader {
    val arrayHeaderByteCount: Int = 8
  }

  private case class ArrayHeader(size: Int, typeIndex: Int) {
    val toBytes: Array[Byte] = Utils.int2Bytes(size) ++ Utils.int2Bytes(typeIndex)
    val byteCount: Int = toBytes.length
    assert(byteCount == ArrayHeader.arrayHeaderByteCount)
  }

  private object InstanceClassHeader {
    val instanceClassHeaderSize: Int = 4
  }

  private case class InstanceClassHeader(classId: ClassId) {
    val toBytes: Array[Byte] = Utils.int2Bytes(classId)
    val byteCount: Int = toBytes.length
    assert(byteCount == InstanceClassHeader.instanceClassHeaderSize)
  }

}

class Heap(size: Int) {
  private val heap: Array[Byte] = Array.fill[Byte](size)(0)
  // too optim, use an hashmap to avoir duplic
  private val arrayTypeIndex: mutable.ArrayBuffer[FType.FTypeArray] = mutable.ArrayBuffer.empty
  private var nextSlot: Int = 1
  private val literalPool: mutable.Map[String, Heap.Address] = mutable.Map.empty
  private val arrayAddresses: mutable.Set[Heap.Address] = mutable.Set.empty

  def isArray(addr: Heap.Address): Boolean = arrayAddresses.contains(addr)

  def toArrayForDebug: Array[Byte] = heap

  private def writeNew(bytes: Array[Byte]): Heap.Address = {
    val ref = nextSlot
    bytes.foreach { b =>
      heap(nextSlot) = b
      nextSlot += 1
    }
    Heap.Address(ref)
  }

  def storeStringLiteral(l: String, classloader: FClassLoader, objectClazz: Clazz): Heap.Address = {
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
        val stRef = new_(stClazz)
        setField(stClazz, fieldRef, stRef, ref, classloader, objectClazz)
        literalPool(l) = stRef
        stRef

    }
  }

  def new_(clazz: InstanceClazz): Heap.Address = {
    val header = InstanceClassHeader(clazz.classId)
    val clazzBytes: Array[Byte] =
      header.toBytes ++ (clazz.allFields.values.toList ++ clazz.secretInstanceField.values.toList).sortBy(
        _.fieldByteIndex
      ).flatMap(f => FValue.default(f.type_).toBytes).toArray
    writeNew(clazzBytes)
  }

  def allocateArray(type_ : FType.FTypeArray, count: Int): Heap.Address = {
    val bytes: Array[Byte] =
      Array.fill[Array[Byte]](count * type_.elementType.byteCount)(type_.elementType.default.toBytes).flatten
    val typeIdx = arrayTypeIndex.length
    arrayTypeIndex += type_
    val header = ArrayHeader(count, typeIdx)
    val addr = writeNew(header.toBytes ++ bytes)
    arrayAddresses += addr
    addr
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

  private def readFieldBytes(objRef: Heap.Address, field: InstanceField): Array[Byte] =
    readBytes(objRef + field.fieldByteIndex, field.type_.byteCount)

  private def writeFieldBytes(objRef: Heap.Address, field: InstanceField, value: FValue): Unit =
    writeBytes(objRef + field.fieldByteIndex, value.toBytes)

  def writeElementInArr(arrRef: Heap.Address, index: Int, value: FValue): Unit = {
    val elemByteCount = getArrType(arrRef).elementType.byteCount
    val addr = arrRef + ArrayHeader.arrayHeaderByteCount + (index * elemByteCount)
    writeBytes(addr, value.toBytes)
  }

  def copyArrayRange(src: Heap.Address, srcPos: Int, dest: Heap.Address, destPos: Int, length: Int): Unit = {
    val elemByteCount = getArrType(src).elementType.byteCount
    val bytes = readBytes(src + ArrayHeader.arrayHeaderByteCount + (srcPos * elemByteCount), length * elemByteCount)
    writeBytes(dest + ArrayHeader.arrayHeaderByteCount + (destPos * elemByteCount), bytes)
  }

  def getRefAt(objRef: Heap.Address, offset: Long): Heap.Address =
    Heap.Address(Utils.bytes2Int(readBytes(objRef + offset.toInt, Heap.addressByteCount)))

  def putRefAt(objRef: Heap.Address, offset: Long, value: Heap.Address): Unit =
    writeBytes(objRef + offset.toInt, Utils.int2Bytes(value.toInt))

  def getIntAt(objRef: Heap.Address, offset: Long): Int =
    Utils.bytes2Int(readBytes(objRef + offset.toInt, 4))

  def putIntAt(objRef: Heap.Address, offset: Long, value: Int): Unit =
    writeBytes(objRef + offset.toInt, Utils.int2Bytes(value))

  def readString(stringRef: FValue.FValueClassRef, classLoader: FClassLoader): String = {
    val (stClazz, valueField) = WKString.valueField(classLoader)
    val arrRef = getField(stClazz, valueField, stringRef.toHeapAddr, classLoader) match {
      case arr: FValue.FValueArrayRef => arr
      case _ => throw new Exception(s"Field value of ${WKString.className} is expected to be an Array ref")
    }
    val array = getArray(FType.FTypeArray.of(FTypeChar), arrRef.toHeapAddr)
    String(array, StandardCharsets.UTF_16BE)
  }

  def getArrayInt(objRef: Heap.Address, idx: Int): Int = {
    val byteCount = FType.FTypeInt.byteCount
    Utils.bytes2Int(readBytes(objRef + ArrayHeader.arrayHeaderByteCount + byteCount * idx, byteCount))
  }

  def getArrayChar(objRef: Heap.Address, idx: Int): Char = {
    val byteCount = FTypeChar.byteCount
    Utils.bytes2Char(readBytes(objRef + ArrayHeader.arrayHeaderByteCount + byteCount * idx, byteCount))
  }

  def getArrayRef(objRef: Heap.Address, idx: Int): Heap.Address = {
    val byteCount = FTypeArray.byteCount
    Heap.Address(Utils.bytes2Int(readBytes(objRef + ArrayHeader.arrayHeaderByteCount + byteCount * idx, byteCount)))
  }

  def getArray(array: FType.FTypeArray, objRef: Heap.Address): Array[Byte] = {
    val arrSize = getArrayLen(objRef)
    val byteCount = arrSize * array.elementType.byteCount
    readBytes(objRef + ArrayHeader.arrayHeaderByteCount, byteCount)
  }

  private def getArrayHeader(objRef: Heap.Address): ArrayHeader = {
    val arrSize = Utils.bytes2Int(readBytes(objRef, 4))
    val arrTypeIdx = Utils.bytes2Int(readBytes(objRef + 4, 4))
    ArrayHeader(arrSize, arrTypeIdx)
  }

  private def getInstanceClassHeader(objRef: Heap.Address): InstanceClassHeader = {
    val classId = Utils.bytes2Int(readBytes(objRef, InstanceClassHeader.instanceClassHeaderSize))
    InstanceClassHeader(classId)
  }

  def getObjectClazz(objRef: Heap.Address, classLoader: FClassLoader): Clazz =
    classLoader.getClazzById(this.getInstanceClassHeader(objRef).classId)

  def getArrayLen(objRef: Heap.Address): Int =
    getArrayHeader(objRef).size

  def getArrType(arrRef: Heap.Address): FType.FTypeArray =
    arrayTypeIndex(getArrayHeader(arrRef).typeIndex)

  def getSecretFieldInt(field: SecretInstanceField, objRef: Heap.Address): Int =
    Utils.bytes2Int(readFieldBytes(objRef, field))

  def setSecretField(field: SecretInstanceField, objRef: Heap.Address, value: FValue): Unit =
    writeFieldBytes(objRef, field, value)

  def getField(clazz: InstanceClazz, field: InstanceField, objRef: Heap.Address, classLoader: FClassLoader): FValue = {
    val bytes = readFieldBytes(objRef, field)
    FValue.fromBytes(field.type_, bytes) match {
      // this code handle the case where the type field is actually a subtype
      case FValue.FValueClassRef(Some(classRef)) =>
        val subClassId = this.getInstanceClassHeader(classRef.addr).classId
        val subClazz = classLoader.getClazzById(subClassId) match {
          case clazz: InstanceClazz => clazz
          case _ => ???
        }
        FValueClassRef.of(classRef.addr, subClazz.name)
      case v => v
    }
  }

  def setField(
    clazz: InstanceClazz,
    field: InstanceField,
    objRef: Heap.Address,
    value: FValue,
    classLoader: FClassLoader,
    objectClazz: Clazz
  ): Unit = {
    val isNullRef = value match {
      case FValue.FValueClassRef(None) | FValue.FValueArrayRef(None) => true
      case _ => false
    }
    val valueToWrite: FValue = if (isNullRef) {
      value
    } else {
      (field.type_, value) match {
        case (t: FType.FTypeClassRef, sRef: FValueRef) if sRef.getType.className != t.className =>
          val objInstanceClazz = objectClazz match {
            case c: InstanceClazz => c
            case other => throw new RuntimeException(s"expected an instance class for Object, got $other")
          }
          if !Assignability.isAssignable(sRef, t, classLoader, objInstanceClazz) then
            throw new RuntimeException(s"field type ${field.type_} is a different type than ${value.getType}")
          value
        // On the operand stack/locals, boolean/byte/short/char all collapse to the "int"
        // computational type (see FValue.asInt), so putfield/putstatic hand us an FValueInt
        // for those field types instead of the narrower FValue variant.
        case (FType.FTypeBoolean | FType.FTypeByte | FType.FTypeShort | FType.FTypeChar, i: FValue.FValueInt) =>
          FValue.narrowInt(field.type_, i)
        case _ =>
          if value.getType != field.type_ then
            throw new RuntimeException(s"field type ${field.type_} is a different type than ${value.getType}")
          value
      }
    }
    writeFieldBytes(objRef, field, valueToWrite)
  }

}
