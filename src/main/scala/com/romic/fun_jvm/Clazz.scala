package com.romic.fun_jvm

import Clazz.{MethodDescriptor, MethodName, StaticFieldDescriptor, StaticFieldName}
import com.romic.fun_jvm.FType.Void
import com.romic.fun_jvm.Heap.Address
import com.romic.fun_jvm.mirror.ClassClazz
import com.romic.fun_jvm.utils.DescriptorHelper
import com.romic.fun_jvm.utils.Utils.{UInt, UShort}

import scala.annotation.tailrec
import scala.collection.mutable

object Clazz {
  type StaticFieldName = String
  type StaticFieldDescriptor = String
  type ClassName = String

  type MethodName = String

  object MethodDescriptor {
    def void: MethodDescriptor = MethodDescriptor(List.empty, Void)

    def parseMethodDescriptor(descriptor: String): MethodDescriptor = {
      val closingParen = descriptor.indexOf(')')
      val paramsStr = descriptor.substring(1, closingParen)
      val returnStr = descriptor.substring(closingParen + 1)

      @tailrec
      def parseParams(i: Int, acc: List[FType]): List[FType] = {
        if (i < paramsStr.length) {
          val (next, nextI) = FType.parseOne(paramsStr, i)
          parseParams(nextI, acc :+ next)
        } else acc
      }

      val returnType = if (returnStr == "V") FType.Void else FType.parseOne(returnStr, 0)._1

      MethodDescriptor(parseParams(0, Nil), returnType)
    }

  }

  case class MethodDescriptor(param: List[FType], return_ : FType)
  case class MethodId(className: ClassName, methodName: MethodName, descriptor: MethodDescriptor)

}

case class ExceptionTableEntry(startPc: UShort, endPc: UShort, handlerPc: UShort, catchType: Option[PoolEntry.CONSTANT_Class_info])

sealed trait Method {
  def methodName: MethodName

  def methodDescriptor: MethodDescriptor
}

case class JvmMethod(
                      methodName: MethodName,
                      methodDescriptor: MethodDescriptor,
                      maxStack: UShort,
                      maxLocals: UShort,
                      code: Vector[Byte],
                      exceptionTable: List[ExceptionTableEntry]) extends Method

case class NativeMethod(methodName: MethodName,
                        methodDescriptor: MethodDescriptor) extends Method


object PoolEntry {
  extension (c: CONSTANT_Class_info) {
    def resolveClazz(classLoader: ClassLoader): Clazz = {
      if (c.maybeResolved.isEmpty) {
        c.maybeResolved = Some(classLoader.getClass(c.name.value))
      }
      c.maybeResolved.get
    }
  }

  extension (ref: CONSTANT_Methodref_info) {
    def resolveMethod(classLoader: ClassLoader): (Clazz, Method) = {
      val clazz = ref.clazz.resolveClazz(classLoader)
      if (ref.maybeResolved.isEmpty) {
        ref.maybeResolved = Some(clazz.methods(ref.nameAndType.toTuple))
      }
      (clazz, ref.maybeResolved.get)
    }
  }

  extension (c: CONSTANT_NameAndType_info) {
    def toTuple: (String, MethodDescriptor) = (c.name.value, DescriptorHelper.parseMethodDescriptor(c.descriptor.value))
    def toTupleSt: (String, String) = (c.name.value, c.descriptor.value)
  }
  
  extension (s: CONSTANT_String_info) {
    def resolveRef(heap: Heap): Heap.Address = {
      s.maybeRef match {
        case Some(a) => a
        case None =>
          val a = heap.storeStringLiteral(s.string.value)
          s.maybeRef = Some(a)
          a
      }
    }
  }
}

enum PoolEntry:
  case CONSTANT_Class_info(name: PoolEntry.CONSTANT_Utf8_info, var maybeResolved: Option[Clazz])
  case CONSTANT_Fieldref_info(clazz: PoolEntry.CONSTANT_Class_info, nameAndType: PoolEntry.CONSTANT_NameAndType_info)
  case CONSTANT_Methodref_info(clazz: PoolEntry.CONSTANT_Class_info, nameAndType: PoolEntry.CONSTANT_NameAndType_info, var maybeResolved: Option[Method])
  case CONSTANT_InterfaceMethodref_info(clazz: PoolEntry.CONSTANT_Class_info, nameAndType: PoolEntry.CONSTANT_NameAndType_info)
  case CONSTANT_String_info(string: PoolEntry.CONSTANT_Utf8_info, var maybeRef: Option[Heap.Address])
  case CONSTANT_Integer_info(value: Int)
  case CONSTANT_Float_info(value: Float)
  case CONSTANT_Long_info(value: Long)
  case CONSTANT_Double_info(value: Double)
  case CONSTANT_NameAndType_info(name: PoolEntry.CONSTANT_Utf8_info, descriptor: PoolEntry.CONSTANT_Utf8_info)
  case CONSTANT_Utf8_info(value: String)
  case CONSTANT_MethodHandle_info(referenceKind: Byte, reference: PoolEntry) // reference target type (Fieldref/Methodref/InterfaceMethodref) depends on referenceKind, so it stays generic
  case CONSTANT_MethodType_info(descriptor: PoolEntry.CONSTANT_Utf8_info)
  case CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex: UShort, nameAndType: PoolEntry.CONSTANT_NameAndType_info) // bootstrapMethodAttrIndex indexes the BootstrapMethods attribute, not the constant pool

object RuntimeConstantPool {
  def empty: RuntimeConstantPool = new RuntimeConstantPool(Array.empty)

  enum LongOrDouble:
    case Double_(value: Double)
    case Long_(value: Long)

  enum IntFloatOrRef:
    case Int_(value: Int)
    case Float_(value: Float)
    case StringRef(value: PoolEntry.CONSTANT_String_info)
    case ClassRef(value: PoolEntry.CONSTANT_Class_info)
}

class RuntimeConstantPool(entries: Array[Option[PoolEntry]]) {
  private def get(index: Int): Option[PoolEntry] =
    if index >= 0 && index < entries.length then entries(index) else None

  def size: Int = entries.length

  def toVector: Vector[Option[PoolEntry]] = entries.toVector

  def debug: String = {
    entries.zipWithIndex
      .collect { case (Some(e), i) => s"  [$i] = $e" }
      .mkString("ConstantPool(\n", "\n", "\n)")
  }

  def resolveIntFlotOrRef(index: UShort): RuntimeConstantPool.IntFloatOrRef =
    get(index.toInt) match {
      case Some(entry: PoolEntry.CONSTANT_Integer_info) => RuntimeConstantPool.IntFloatOrRef.Int_(entry.value)
      case Some(entry: PoolEntry.CONSTANT_Float_info) => RuntimeConstantPool.IntFloatOrRef.Float_(entry.value)
      case Some(entry: PoolEntry.CONSTANT_String_info) => RuntimeConstantPool.IntFloatOrRef.StringRef(entry)
      case Some(entry: PoolEntry.CONSTANT_Class_info) => RuntimeConstantPool.IntFloatOrRef.ClassRef(entry)
      case Some(_: PoolEntry.CONSTANT_MethodHandle_info) =>
        throw new RuntimeException("ldc on a MethodHandle constant is not implemented yet")
      case Some(_: PoolEntry.CONSTANT_MethodType_info) =>
        throw new RuntimeException("ldc on a MethodType constant is not implemented yet")
      case Some(wut) => throw new RuntimeException(s"was expected int, float, string, class, method handle or method type, got $wut")
      case None => throw new RuntimeException(s"No pool entry at index $index")
    }

  def resolveLongOrDouble(index: UShort): RuntimeConstantPool.LongOrDouble =
    get(index.toInt) match {
      case Some(entry: PoolEntry.CONSTANT_Long_info) => RuntimeConstantPool.LongOrDouble.Long_(entry.value)
      case Some(entry: PoolEntry.CONSTANT_Double_info) => RuntimeConstantPool.LongOrDouble.Double_(entry.value)
      case Some(wut) => throw new RuntimeException(s"was expected double or long, got $wut")
      case None => throw new RuntimeException(s"No pool entry at index $index")
    }

  def resolveClass(index: UShort): PoolEntry.CONSTANT_Class_info =
    get(index.toInt) match {
      case Some(entry: PoolEntry.CONSTANT_Class_info) => entry
      case Some(wut) => throw new RuntimeException(s"was expected class info, got $wut")
      case None => throw new RuntimeException(s"no constant with idx $index")
    }

  def resolveUtf8(index: UShort): PoolEntry.CONSTANT_Utf8_info =
    get(index.toInt) match {
      case Some(entry: PoolEntry.CONSTANT_Utf8_info) => entry
      case Some(wut) => throw new RuntimeException(s"was expected utf8, got $wut")
      case None => throw new RuntimeException(s"no constant with idx $index")
    }

  def resolveFieldref(index: UShort): PoolEntry.CONSTANT_Fieldref_info =
    get(index.toInt) match {
      case Some(entry: PoolEntry.CONSTANT_Fieldref_info) => entry
      case Some(wut) => throw new RuntimeException(s"was expected fieldref, got $wut")
      case None => throw new RuntimeException(s"no constant with idx $index")
    }

  def resolveMethodRef(index: UShort): PoolEntry.CONSTANT_Methodref_info =
    get(index.toInt) match {
      case Some(entry: PoolEntry.CONSTANT_Methodref_info) => entry
      case Some(wut) => throw new RuntimeException(s"was expected methodRef, got $wut")
      case None => throw new RuntimeException(s"no constant with idx $index")
    }
}

class Clazz(val name: String,
            val staticFields: mutable.Map[(StaticFieldName, StaticFieldDescriptor), FValue], val constantPool: RuntimeConstantPool, val jvmMethods: Map[(MethodName, MethodDescriptor), JvmMethod],
            val nativeMethods: Map[(MethodName, MethodDescriptor), NativeMethod],
            val instanceFields: List[(StaticFieldName, StaticFieldDescriptor)],
            heap: Heap) {
  val maybeInitMet: Option[JvmMethod] = jvmMethods.get(("<init>", MethodDescriptor.void))
  val maybeClinitMet: Option[JvmMethod] = jvmMethods.get(("<clinit>", MethodDescriptor.void))
  val methods: Map[(MethodName, MethodDescriptor), Method] = jvmMethods ++ nativeMethods

  private var classMirror: Option[Heap.Address] = None

  def getClassMirror: Address = classMirror match {
    case Some(a) => a
    case None =>
      val addr = heap.storeNew(ClassClazz.getClassClazz(heap))
      classMirror = Some(addr)
      addr
  }

}