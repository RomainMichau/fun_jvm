package com.romic.fun_jvm.classparser

import ClassFileInfo.ConstantPool.{resolveClass, resolveUtf8}
import ClassFileInfo.FieldAccessFlag.ACC_STATIC
import ClassFileInfo.RawConstantPool.resolvePoolEntry
import ClassFileInfo.{ClassFileProperties, ConstantPool, MethodDescriptorStr, MethodName, read2Bytes}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.{catsSyntaxTuple2Semigroupal, catsSyntaxTuple3Semigroupal, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedId, toTraverseOps}
import com.romic.fun_jvm.Clazz.MethodDescriptor
import com.romic.fun_jvm.utils.Utils.*
import com.romic.fun_jvm.{Clazz, FValue, JvmMethod, Method, ModifiedUtf8Decoder, NativeMethod, RuntimeConstantPool, ExceptionTableEntry as RuntimeExceptionTableEntry, PoolEntry as RuntimePoolEntry}

import java.nio.file.{Files, Paths}
import scala.collection.mutable

object ClassFileInfo {

  private[classparser] type Result[A] = ValidatedNel[String, A]

  private[classparser] object ConstantPoolTag {
    val CONSTANT_Utf8 = 1
    val CONSTANT_Integer = 3
    val CONSTANT_Float = 4
    val CONSTANT_Long = 5
    val CONSTANT_Double = 6
    val CONSTANT_Class = 7
    val CONSTANT_String = 8
    val CONSTANT_Fieldref = 9
    val CONSTANT_Methodref = 10
    val CONSTANT_InterfaceMethodref = 11
    val CONSTANT_NameAndType = 12
    val CONSTANT_MethodHandle = 15
    val CONSTANT_MethodType = 16
    val CONSTANT_Dynamic = 17
    val CONSTANT_InvokeDynamic = 18
    val CONSTANT_Module = 19
    val CONSTANT_Package = 20
  }

  private[classparser] object RawConstantPool {
    def apply(v: Vector[Option[PoolEntryRaw]]): RawConstantPool = v

    extension (rawPool: RawConstantPool) {
      def toVector: Vector[Option[PoolEntryRaw]] = rawPool

      def resolveUtf8(index: UShort): Result[PoolEntry.CONSTANT_Utf8_info] =
        rawPool.lift(index.toInt).flatten match {
          case Some(PoolEntryRaw.CONSTANT_Utf8_info(value)) => PoolEntry.CONSTANT_Utf8_info(value).validNel
          case Some(other) => s"Expected CONSTANT_Utf8 at index ${index}, got $other".invalidNel
          case None => s"No constant pool entry at index ${index}".invalidNel
        }

      def resolveClass(index: UShort): Result[PoolEntry.CONSTANT_Class_info] =
        rawPool.lift(index.toInt).flatten match {
          case Some(PoolEntryRaw.CONSTANT_Class_info(nameIndex)) =>
            resolveUtf8(nameIndex).map(PoolEntry.CONSTANT_Class_info.apply)
          case Some(other) => s"Expected CONSTANT_Class at index ${index}, got $other".invalidNel
          case None => s"No constant pool entry at index ${index}".invalidNel
        }

      def resolveNameAndType(index: UShort): Result[PoolEntry.CONSTANT_NameAndType_info] =
        rawPool.lift(index.toInt).flatten match {
          case Some(PoolEntryRaw.CONSTANT_NameAndType_info(nameIndex, descriptorIndex)) =>
            (resolveUtf8(nameIndex), resolveUtf8(descriptorIndex)).mapN(PoolEntry.CONSTANT_NameAndType_info.apply)
          case Some(other) => s"Expected CONSTANT_NameAndType at index ${index}, got $other".invalidNel
          case None => s"No constant pool entry at index ${index}".invalidNel
        }

      def resolveRawEntry(raw: PoolEntryRaw): Result[PoolEntry] = raw match {
        case PoolEntryRaw.CONSTANT_Class_info(nameIndex) =>
          resolveUtf8(nameIndex).map(PoolEntry.CONSTANT_Class_info.apply)
        case PoolEntryRaw.CONSTANT_Fieldref_info(classIndex, nameAndTypeIndex) =>
          (resolveClass(classIndex), resolveNameAndType(nameAndTypeIndex)).mapN(PoolEntry.CONSTANT_Fieldref_info.apply)
        case PoolEntryRaw.CONSTANT_Methodref_info(classIndex, nameAndTypeIndex) =>
          (resolveClass(classIndex), resolveNameAndType(nameAndTypeIndex)).mapN(PoolEntry.CONSTANT_Methodref_info.apply)
        case PoolEntryRaw.CONSTANT_InterfaceMethodref_info(classIndex, nameAndTypeIndex) =>
          (resolveClass(classIndex), resolveNameAndType(nameAndTypeIndex)).mapN(PoolEntry.CONSTANT_InterfaceMethodref_info.apply)
        case PoolEntryRaw.CONSTANT_String_info(stringIndex) =>
          resolveUtf8(stringIndex).map(PoolEntry.CONSTANT_String_info.apply)
        case PoolEntryRaw.CONSTANT_Integer_info(value) => PoolEntry.CONSTANT_Integer_info(value).validNel
        case PoolEntryRaw.CONSTANT_Float_info(value) => PoolEntry.CONSTANT_Float_info(value).validNel
        case PoolEntryRaw.CONSTANT_Long_info(value) => PoolEntry.CONSTANT_Long_info(value).validNel
        case PoolEntryRaw.CONSTANT_Double_info(value) => PoolEntry.CONSTANT_Double_info(value).validNel
        case PoolEntryRaw.CONSTANT_NameAndType_info(nameIndex, descriptorIndex) =>
          (resolveUtf8(nameIndex), resolveUtf8(descriptorIndex)).mapN(PoolEntry.CONSTANT_NameAndType_info.apply)
        case PoolEntryRaw.CONSTANT_Utf8_info(value) => PoolEntry.CONSTANT_Utf8_info(value).validNel
        case PoolEntryRaw.CONSTANT_MethodHandle_info(referenceKind, referenceIndex) =>
          rawPool.lift(referenceIndex.toInt).flatten match {
            case Some(referenced) => resolveRawEntry(referenced).map(PoolEntry.CONSTANT_MethodHandle_info(referenceKind, _))
            case None => s"No constant pool entry at index ${referenceIndex}".invalidNel
          }
        case PoolEntryRaw.CONSTANT_MethodType_info(descriptorIndex) =>
          resolveUtf8(descriptorIndex).map(PoolEntry.CONSTANT_MethodType_info.apply)
        case PoolEntryRaw.CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex, nameAndTypeIndex) =>
          resolveNameAndType(nameAndTypeIndex).map(PoolEntry.CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex, _))
      }

      def resolvePoolEntry: Result[ConstantPool] =
        rawPool.traverse {
          case None => None.validNel
          case Some(rawEntry) => rawPool.resolveRawEntry(rawEntry).map(Some(_))
        }
    }
  }

  private[classparser] opaque type RawConstantPool = Vector[Option[PoolEntryRaw]]

  // PoolEntry that use index to point to others entry
  private[classparser] enum PoolEntryRaw:
    case CONSTANT_Class_info(nameIndex: UShort)
    case CONSTANT_Fieldref_info(classIndex: UShort, nameAndTypeIndex: UShort)
    case CONSTANT_Methodref_info(classIndex: UShort, nameAndTypeIndex: UShort)
    case CONSTANT_InterfaceMethodref_info(classIndex: UShort, nameAndTypeIndex: UShort)
    case CONSTANT_String_info(stringIndex: UShort)
    case CONSTANT_Integer_info(value: Int) // u4
    case CONSTANT_Float_info(value: Float) // u4
    case CONSTANT_Long_info(value: Long) // u8, takes two constant pool slots
    case CONSTANT_Double_info(value: Double) // u8, takes two constant pool slots
    case CONSTANT_NameAndType_info(nameIndex: UShort, descriptorIndex: UShort)
    case CONSTANT_Utf8_info(value: String)
    case CONSTANT_MethodHandle_info(referenceKind: Byte, referenceIndex: UShort) // referenceKind is u1
    case CONSTANT_MethodType_info(descriptorIndex: UShort)
    case CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex: UShort, nameAndTypeIndex: UShort)

  // PoolEntry with index resolved to directly point toward other entry
  private[classparser] enum PoolEntry:
    case CONSTANT_Class_info(name: PoolEntry.CONSTANT_Utf8_info)
    case CONSTANT_Fieldref_info(clazz: PoolEntry.CONSTANT_Class_info, nameAndType: PoolEntry.CONSTANT_NameAndType_info)
    case CONSTANT_Methodref_info(clazz: PoolEntry.CONSTANT_Class_info, nameAndType: PoolEntry.CONSTANT_NameAndType_info)
    case CONSTANT_InterfaceMethodref_info(clazz: PoolEntry.CONSTANT_Class_info, nameAndType: PoolEntry.CONSTANT_NameAndType_info)
    case CONSTANT_String_info(string: PoolEntry.CONSTANT_Utf8_info)
    case CONSTANT_Integer_info(value: Int)
    case CONSTANT_Float_info(value: Float)
    case CONSTANT_Long_info(value: Long)
    case CONSTANT_Double_info(value: Double)
    case CONSTANT_NameAndType_info(name: PoolEntry.CONSTANT_Utf8_info, descriptor: PoolEntry.CONSTANT_Utf8_info)
    case CONSTANT_Utf8_info(value: String)
    case CONSTANT_MethodHandle_info(referenceKind: Byte, reference: PoolEntry) // reference target type (Fieldref/Methodref/InterfaceMethodref) depends on referenceKind, so it stays generic
    case CONSTANT_MethodType_info(descriptor: PoolEntry.CONSTANT_Utf8_info)
    case CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex: UShort, nameAndType: PoolEntry.CONSTANT_NameAndType_info) // bootstrapMethodAttrIndex indexes the BootstrapMethods attribute, not the constant pool

  private[classparser] case class AttributeHeader(attributeName: String, len: UInt)

  // the {start_pc, end_pc, handler_pc, catch_type} rows of Code_attribute's exception_table; catchType is None for catch_type == 0 ("catch all", used for finally)
  private[classparser] case class ExceptionTableEntry(startPc: UShort, endPc: UShort, handlerPc: UShort, catchType: Option[PoolEntry.CONSTANT_Class_info])

  private[classparser] object Attribute {
    def readConstantValue(byte: Array[Byte], offset: Int, pool: ConstantPool): Result[ConstantValue] = read2Bytes(byte, offset)
      .map { idx =>
        val value = pool(idx.toInt).get
        ConstantValue(value)
      }

    def readCodeBody(bytes: Array[Byte], offset: Int, pool: ConstantPool, len: Int): Result[Attribute.Code] =
      read2Bytes(bytes, offset, "code attr max_stack").andThen { maxStack =>
        read2Bytes(bytes, offset + 2, "code attr max_locals").andThen { maxLocals =>
          readUInt(bytes, offset + 4, "code attr code_length").andThen { codeLen =>
            val codeStart = offset + 8
            val codeEnd = codeStart + codeLen.toInt
            val code = bytes.slice(codeStart, codeEnd)
            read2Bytes(bytes, codeEnd, "code attr exception_table_length").andThen { exceptionTableLen =>
              val exceptionTableStart = codeEnd + 2
              (0 until exceptionTableLen.toInt).toList
                .foldLeft((List.empty[ExceptionTableEntry], exceptionTableStart).validNel[String]) { case (accResult, _) =>
                  accResult.andThen { (acc, entryOffset) =>
                    read2Bytes(bytes, entryOffset, "exception_table start_pc").andThen { startPc =>
                      read2Bytes(bytes, entryOffset + 2, "exception_table end_pc").andThen { endPc =>
                        read2Bytes(bytes, entryOffset + 4, "exception_table handler_pc").andThen { handlerPc =>
                          read2Bytes(bytes, entryOffset + 6, "exception_table catch_type").andThen { catchTypeIdx =>
                            val catchType =
                              if catchTypeIdx.toInt == 0 then None.validNel
                              else pool.resolveClass(catchTypeIdx).map(Some(_))
                            catchType.map { ct =>
                              (acc :+ ExceptionTableEntry(startPc, endPc, handlerPc, ct), entryOffset + 8)
                            }
                          }
                        }
                      }
                    }
                  }
                }
                .andThen { (exceptionTable, attributesOffset) =>
                  readAttributes(bytes, attributesOffset, pool).map { (attributes, _) =>
                    Attribute.Code(maxStack, maxLocals, code.toVector, exceptionTable, attributes, UInt(len.toLong))
                  }
                }
            }
          }
        }
      }

    def readExceptionsBody(bytes: Array[Byte], offset: Int, pool: ConstantPool, len: Int): Result[Attribute.Exceptions] =
      read2Bytes(bytes, offset, "exceptions attr number_of_exceptions").andThen { numberOfExceptions =>
        (0 until numberOfExceptions.toInt).toList
          .traverse { i => read2Bytes(bytes, offset + 2 + i * 2, s"exceptions attr exception_index_table[$i]").andThen(pool.resolveClass) }
          .map { exceptionIndexTable => Attribute.Exceptions(exceptionIndexTable, UInt(len.toLong)) }
      }

  }

  private[classparser] enum Attribute(val header: AttributeHeader):
    case ConstantValue(poolEntry: PoolEntry) extends Attribute(AttributeHeader("ConstantValue", UInt(2)))

    // Code/Exceptions are variable-length (unlike ConstantValue's spec-fixed 2 bytes), so `length` is a real field carrying whatever attribute_length was actually read, not a hardcoded constant
    case Code(maxStack: UShort,
              maxLocals: UShort,
              code: Vector[Byte],
              exceptionTable: List[ExceptionTableEntry],
              attributes: List[Attribute],
              length: UInt) extends Attribute(AttributeHeader("Code", length))

    case Exceptions(exceptionIndexTable: List[PoolEntry.CONSTANT_Class_info],
                    length: UInt) extends Attribute(AttributeHeader("Exceptions", length))

  private[classparser] object ConstantPool {
    def apply(v: Vector[Option[PoolEntry]]): ConstantPool = v

    enum LongOrDouble:
      case Double_(d: Double)
      case Long_(d: Long)

    extension (pool: ConstantPool) {
      def toVector: Vector[Option[PoolEntry]] = pool


      def resolveLongOrDouble(index: UShort): Result[LongOrDouble] =
        pool.lift(index.toInt).flatten match {
          case Some(entry: PoolEntry.CONSTANT_Long_info) => LongOrDouble.Long_(entry.value).validNel
          case Some(entry: PoolEntry.CONSTANT_Double_info) => LongOrDouble.Double_(entry.value).validNel
          case Some(wut) =>
            s"was expected double or long, got $wut".invalidNel
          case None =>
            s"No pool entry at index $index".invalidNel
        }


      def resolveClass(index: UShort): Result[PoolEntry.CONSTANT_Class_info] =
        pool.lift(index.toInt).flatten match {
          case Some(entry: PoolEntry.CONSTANT_Class_info) => entry.validNel
          case Some(wut) => s"was expected class info, got $wut".invalidNel
          case None => s"no constant with idx $index".invalidNel
        }

      def resolveUtf8(index: UShort): Result[PoolEntry.CONSTANT_Utf8_info] =
        pool.lift(index.toInt).flatten match {
          case Some(entry: PoolEntry.CONSTANT_Utf8_info) => entry.validNel
          case Some(wut) => s"was expected utf8, got $wut".invalidNel
          case None => s"no constant with idx $index".invalidNel
        }
      def size: Int = pool.size

    }

  }

  private[classparser] opaque type ConstantPool = Vector[Option[PoolEntry]]

  private[classparser] def magicNumberMatch(bytes: Array[Byte]): Boolean = {
    bytes.take(4).map(_ & 0xff) sameElements Array(0xca, 0xfe, 0xba, 0xbe)
  }

  private[classparser] def read2Bytes(bytes: Array[Byte], offset: Int, label: String = "smthing"): Result[UShort] = {
    bytes.toList.slice(offset, offset + 2) match {
      case a :: b :: _ => UShort(((a & 0xff) << 8) | (b & 0xff)).valid
      case _ => s"Unable to get $label".invalidNel
    }
  }

  private[classparser] def readUInt(bytes: Array[Byte], offset: Int, label: String = "smthing"): Result[UInt] = {
    bytes.toList.slice(offset, offset + 4) match {
      case a :: b :: c :: d :: _ =>
        UInt((a & 0xffL) << 24 | (b & 0xffL) << 16 | (c & 0xffL) << 8 | (d & 0xffL)).valid
      case _ => s"Unable to get $label".invalidNel
    }
  }

  private[classparser] def readInt(bytes: Array[Byte], offset: Int, label: String = "smthing"): Result[Int] = {
    bytes.toList.slice(offset, offset + 4) match {
      case a :: b :: c :: d :: _ => (((a & 0xff) << 24) | ((b & 0xff) << 16) | ((c & 0xff) << 8) | (d & 0xff)).valid
      case _ => s"Unable to get $label".invalidNel
    }
  }


  private[classparser] def readFloat(bytes: Array[Byte], offset: Int, label: String = "smthing"): Result[Float] = {
    readInt(bytes, offset, label).map {
      case 0x7f800000 => Float.PositiveInfinity
      case 0xff800000 => Float.NegativeInfinity
      case nan if (0x7f800001 to 0x7fffffff).contains(nan) ||
        (0xff800001 to 0xffffffff).contains(nan) => Float.NaN
      case bits =>
        val s = if (bits >> 31) == 0 then 1 else -1
        val e = (bits >> 23) & 0xff
        val m = if e == 0 then (bits & 0x7fffff) << 1 else (bits & 0x7fffff) | 0x800000
        //  s · m · 2e-150.
        (s.toFloat * m * math.pow(2, e - 150)).toFloat

    }
  }

  private[classparser] def readLong(bytes: Array[Byte], offset: Int, label: String = "smthing"): Result[Long] = {
    bytes.toList.slice(offset, offset + 8) match {
      case a :: b :: c :: d :: e :: f :: g :: h :: _ => (((a.toLong & 0xff) << 56) | ((b & 0xff) << 48) | ((c & 0xff) << 40) | ((d & 0xff) << 32) |
        ((e & 0xff) << 24) | ((f & 0xff) << 16) | ((g & 0xff) << 8) | (h & 0xff)).valid
      case _ => s"Unable to get $label".invalidNel
    }
  }

  private[classparser] def readDouble(bytes: Array[Byte], offset: Int, label: String = "smthing"): Result[Double] = {
    readLong(bytes, offset, label).map {
      case 0x7ff0000000000000L => Double.PositiveInfinity
      case 0xfff0000000000000L => Double.NegativeInfinity
      case nan if (0x7ff0000000000001L to 0x7fffffffffffffffL).contains(nan) ||
        (0xfff0000000000001L to 0xffffffffffffffffL).contains(nan) => Double.NaN
      case bits =>
        val s = if (bits >> 63) == 0 then 1 else -1
        val e = ((bits >> 52) & 0x7ffL).toInt
        val m = if e == 0 then (bits & 0xfffffffffffffL) << 1 else (bits & 0xfffffffffffffL) | 0x10000000000000L
        //  s · m · 2e-1075.
        s.toDouble * m.toDouble * math.pow(2, e - 1075)
    }
  }

  private[classparser] def majorVersion(bytes: Array[Byte]): Result[UShort] = read2Bytes(bytes, 6, "major")

  private[classparser] def minorVersion(bytes: Array[Byte]): Result[UShort] = read2Bytes(bytes, 4, "minor")

  private[classparser] def getPoolsize(bytes: Array[Byte]): Result[UShort] = read2Bytes(bytes, 8, "pool size")

  private[classparser] type NextOffset = Int

  private[classparser] def readPoolEntry(bytes: Array[Byte], offset: Int): Result[(PoolEntryRaw, NextOffset)] =
    (bytes(offset) & 0xff) match {
      case ConstantPoolTag.CONSTANT_Class => read2Bytes(bytes, offset + 1)
        .map { idx => (PoolEntryRaw.CONSTANT_Class_info(idx), offset + 3) }
      case ConstantPoolTag.CONSTANT_Fieldref =>
        read2Bytes(bytes, offset + 1).andThen {
          classIdx =>
            read2Bytes(bytes, offset + 3).map { name_and_type_index =>
              (PoolEntryRaw.CONSTANT_Fieldref_info(classIdx, name_and_type_index), offset + 5)
            }
        }
      case ConstantPoolTag.CONSTANT_Methodref =>
        read2Bytes(bytes, offset + 1).andThen {
          classIdx =>
            read2Bytes(bytes, offset + 3).map { name_and_type_index =>
              (PoolEntryRaw.CONSTANT_Methodref_info(classIdx, name_and_type_index), offset + 5)
            }
        }
      case ConstantPoolTag.CONSTANT_InterfaceMethodref =>
        read2Bytes(bytes, offset + 1).andThen {
          classIdx =>
            read2Bytes(bytes, offset + 3).map { name_and_type_index =>
              (PoolEntryRaw.CONSTANT_InterfaceMethodref_info(classIdx, name_and_type_index), offset + 5)
            }
        }
      case ConstantPoolTag.CONSTANT_String => read2Bytes(bytes, offset + 1)
        .map { idx => (PoolEntryRaw.CONSTANT_String_info(idx), offset + 3) }
      case ConstantPoolTag.CONSTANT_Integer => readInt(bytes, offset + 1)
        .map { i => (PoolEntryRaw.CONSTANT_Integer_info(i), offset + 5) }
      case ConstantPoolTag.CONSTANT_Float => readFloat(bytes, offset + 1)
        .map { f => (PoolEntryRaw.CONSTANT_Float_info(f), offset + 5) }
      case ConstantPoolTag.CONSTANT_Long => readLong(bytes, offset + 1)
        .map { f => (PoolEntryRaw.CONSTANT_Long_info(f), offset + 9) }

      case ConstantPoolTag.CONSTANT_Double => readDouble(bytes, offset + 1)
        .map { d => (PoolEntryRaw.CONSTANT_Double_info(d), offset + 9) }
      case ConstantPoolTag.CONSTANT_NameAndType => read2Bytes(bytes, offset + 1).andThen {
        nameIdx =>
          read2Bytes(bytes, offset + 3).map { descIdx =>
            (PoolEntryRaw.CONSTANT_NameAndType_info(nameIdx, descIdx), offset + 5)
          }
      }
      case ConstantPoolTag.CONSTANT_Utf8 => read2Bytes(bytes, offset + 1).map {
        len =>
          val str: String = ModifiedUtf8Decoder.decode(bytes, offset + 1, len.toInt)

          (PoolEntryRaw.CONSTANT_Utf8_info(str), offset + 3 + len.toInt)
      }
      case ConstantPoolTag.CONSTANT_MethodHandle =>
        val reference_kind = bytes(offset + 1)
        read2Bytes(bytes, offset + 2)
          .map { idx => (PoolEntryRaw.CONSTANT_MethodHandle_info(reference_kind, idx), offset + 4) }

      case ConstantPoolTag.CONSTANT_MethodType => read2Bytes(bytes, offset + 1)
        .map { idx => (PoolEntryRaw.CONSTANT_MethodType_info(idx), offset + 3) }
      case ConstantPoolTag.CONSTANT_InvokeDynamic => read2Bytes(bytes, offset + 1).andThen {
        bootstrap_method_attr_index =>
          read2Bytes(bytes, offset + 3).map { name_and_type_index =>
            (PoolEntryRaw.CONSTANT_InvokeDynamic_info(bootstrap_method_attr_index, name_and_type_index), offset + 5)
          }
      }
      case wut => throw new RuntimeException(s"Wut is constant type $wut bruh ?")
    }

  private[classparser] def readAllRawPoolEntry(bytes: Array[Byte],
                                               poolSize: Int,
                                               offset: Int = 10,
                                               iter: Int = 0,
                                               poolEntries: RawConstantPool = RawConstantPool(Vector(None))): Result[(RawConstantPool, NextOffset)] = {
    if (iter < poolSize - 1) {
      readPoolEntry(bytes, offset)
        .andThen { (newEntry, nextOffset) =>
          val (newPoolEntries, newIter) = newEntry match {
            case e: PoolEntryRaw.CONSTANT_Double_info => (Vector(Some(e), None), iter + 2)
            case e: PoolEntryRaw.CONSTANT_Long_info => (Vector(Some(e), None), iter + 2)
            case e => (Vector(Some(e)), iter + 1)
          }
          readAllRawPoolEntry(bytes, poolSize, nextOffset, newIter, RawConstantPool(poolEntries.toVector ++ newPoolEntries))
        }
    } else {
      (poolEntries, offset).validNel
    }
  }

  private[classparser] def readAllPoolEntry(bytes: Array[Byte],
                                            poolSize: Int): Result[(ConstantPool, NextOffset)] =
    readAllRawPoolEntry(bytes, poolSize).andThen { (rawEntries, nextOffset) =>
      rawEntries.resolvePoolEntry.map((_, nextOffset))
    }

  private[classparser] enum ClassAccessFlag(val mask: Int):
    case ACC_PUBLIC extends ClassAccessFlag(0x0001)
    case ACC_FINAL extends ClassAccessFlag(0x0010)
    case ACC_SUPER extends ClassAccessFlag(0x0020)
    case ACC_INTERFACE extends ClassAccessFlag(0x0200)
    case ACC_ABSTRACT extends ClassAccessFlag(0x0400)
    case ACC_SYNTHETIC extends ClassAccessFlag(0x1000)
    case ACC_ANNOTATION extends ClassAccessFlag(0x2000)
    case ACC_ENUM extends ClassAccessFlag(0x4000)

  // Table 4.5-A. Field access and property flags
  private[classparser] enum FieldAccessFlag(val mask: Int):
    case ACC_PUBLIC extends FieldAccessFlag(0x0001)
    case ACC_ extends FieldAccessFlag(0x0002)
    case ACC_PROTECTED extends FieldAccessFlag(0x0004)
    case ACC_STATIC extends FieldAccessFlag(0x0008)
    case ACC_FINAL extends FieldAccessFlag(0x0010)
    case ACC_VOLATILE extends FieldAccessFlag(0x0040)
    case ACC_TRANSIENT extends FieldAccessFlag(0x0080)
    case ACC_SYNTHETIC extends FieldAccessFlag(0x1000)
    case ACC_ENUM extends FieldAccessFlag(0x4000)

  private[classparser] case class ClassFileProperties(flags: List[ClassAccessFlag], thisClass: PoolEntry.CONSTANT_Class_info, superClass: Option[PoolEntry.CONSTANT_Class_info], interfaces: List[PoolEntry.CONSTANT_Class_info])

  private[classparser] def readInterfaces(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(List[PoolEntry.CONSTANT_Class_info], NextOffset)] =
    read2Bytes(bytes, offset, "interfaces count").andThen { count =>
      (0 until count.toInt).toList
        .traverse { i => read2Bytes(bytes, offset + 2 + i * 2, s"interface $i").andThen(idx => pool.resolveClass(idx)) }
        .map { interfaces => (interfaces, offset + 2 + count.toInt * 2) }
    }

  private[classparser] def readProperties(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(ClassFileProperties, NextOffset)] = {
    val flags = read2Bytes(bytes, offset, "access flag").map { s =>
      ClassAccessFlag.values.filter(flag => (flag.mask & s.toInt) != 0).toList
    }
    val thisClass = read2Bytes(bytes, offset + 2, "this class").andThen(idx => pool.resolveClass(idx))
    val superClass = read2Bytes(bytes, offset + 4, "super class").andThen { idx =>
      if idx.toInt == 0 then None.validNel else pool.resolveClass(idx).map(Some(_))
    }
    val interfaces = readInterfaces(bytes, offset + 6, pool)
    (flags, thisClass, superClass, interfaces).mapN { (flags, thisClass, superClass, interfaces) =>
      val (interfaceList, nextOffset) = interfaces
      (ClassFileProperties(flags, thisClass, superClass, interfaceList), nextOffset)
    }
  }

  private[classparser] def readAttributeHeader(byte: Array[Byte], offset: Int, pool: ConstantPool): Result[(AttributeHeader, NextOffset)] = {
    read2Bytes(byte, offset).andThen { idx =>
      pool.resolveUtf8(idx).andThen { name =>
        readUInt(byte, offset + 2)
          .map(size => (AttributeHeader(name.value, size), offset + 6))
      }
    }
  }

  private[classparser] case class FieldInfo(accessFlags: List[FieldAccessFlag],
                                            name: PoolEntry.CONSTANT_Utf8_info,
                                            descriptor: PoolEntry.CONSTANT_Utf8_info,
                                            attributes: Set[Attribute])

  private[classparser] def readAttribute(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(Option[Attribute], NextOffset)] = {
    readAttributeHeader(bytes, offset, pool).andThen { (header, nextOffset) =>
      val attr = header.attributeName match {
        case "ConstantValue" => Attribute.readConstantValue(bytes, nextOffset, pool).map(Some(_))
        case "Code" => Attribute.readCodeBody(bytes, nextOffset, pool, header.len.toInt).map(Some(_))
        case "Exceptions" => Attribute.readExceptionsBody(bytes, nextOffset, pool, header.len.toInt).map(Some(_))
        case _ => None.validNel

      }
      attr.map((_, nextOffset + header.len.toLong.toInt))
    }
  }

  private[classparser] def readAttributes(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(List[Attribute], NextOffset)] = {
    read2Bytes(bytes, offset).andThen { attributeCount =>
      (0 until attributeCount.toInt).foldLeft((List.empty[Attribute], offset + 2).validNel[String]) { case (accResult, _) =>
        accResult.andThen { (acc, curOffset) =>
          readAttribute(bytes, curOffset, pool).map { case (maybeAttr, nextOffset) => maybeAttr match {
            case Some(attr) => (acc :+ attr, nextOffset)
            case None => (acc, nextOffset)
          }
          }
        }
      }
    }
  }

  private[classparser] def readAndResolveUtf8Constant(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[PoolEntry.CONSTANT_Utf8_info] = {
    read2Bytes(bytes, offset, s"reading resolvable utf8 constant on offset  $offset").andThen { nameIdx =>
      pool.resolveUtf8(nameIdx)
    }
  }

  private[classparser] def readField(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(FieldInfo, NextOffset)] = {
    read2Bytes(bytes, offset, s"reading field flags on offset  $offset")
      .andThen { fieldAccessFlagRaw =>
        val accessFlags = FieldAccessFlag.values.filter { flag => (flag.mask & fieldAccessFlagRaw.toInt) != 0 }
        readAndResolveUtf8Constant(bytes, offset + 2, pool).andThen { fieldName =>
          readAndResolveUtf8Constant(bytes, offset + 4, pool).andThen { descriptor =>
            readAttributes(bytes, offset + 6, pool).map { (attributes, nextOffset) =>
              (FieldInfo(accessFlags.toList, fieldName, descriptor, attributes.toSet), nextOffset)
            }
          }

        }
      }
  }


  private[classparser] def readFields(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(List[FieldInfo], NextOffset)] = {

    read2Bytes(bytes, offset, "field count").andThen { fieldCount =>
      (0 until fieldCount.toInt).toList.foldLeft((List.empty[FieldInfo], offset + 2).validNel[String]) {
        case (accResult, _) =>
          accResult.andThen { case (fields, currentOffset) =>
            readField(bytes, currentOffset, pool).map { case (field, nextOffset) =>
              (fields :+ field, nextOffset)
            }
          }
      }
    }
  }

  // Table 4.6-A. Method access flags
  private[classparser] enum MethodAccessFlag(val mask: Int):
    case ACC_PUBLIC extends MethodAccessFlag(0x0001)
    case ACC_ extends MethodAccessFlag(0x0002)
    case ACC_PROTECTED extends MethodAccessFlag(0x0004)
    case ACC_STATIC extends MethodAccessFlag(0x0008)
    case ACC_FINAL extends MethodAccessFlag(0x0010)
    case ACC_SYNCHRONIZED extends MethodAccessFlag(0x0020)
    case ACC_BRIDGE extends MethodAccessFlag(0x0040)
    case ACC_VARARGS extends MethodAccessFlag(0x0080)
    case ACC_NATIVE extends MethodAccessFlag(0x0100)
    case ACC_ABSTRACT extends MethodAccessFlag(0x0400)
    case ACC_STRICT extends MethodAccessFlag(0x0800)
    case ACC_SYNTHETIC extends MethodAccessFlag(0x1000)

  private[classparser] case class MethodInfo(accessFlags: Set[MethodAccessFlag],
                                             name: PoolEntry.CONSTANT_Utf8_info,
                                             descriptor: PoolEntry.CONSTANT_Utf8_info,
                                             attributes: List[Attribute]) {
    val code = attributes.collectFirst {
      case x: Attribute.Code => x
    }

    val isNative: Boolean = accessFlags.contains(MethodAccessFlag.ACC_NATIVE)
  }

  private[classparser] def readMethod(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(MethodInfo, NextOffset)] = {
    read2Bytes(bytes, offset, s"reading method flags on offset  $offset")
      .andThen { methodAccessFlagRaw =>
        val accessFlags = MethodAccessFlag.values.filter { flag => (flag.mask & methodAccessFlagRaw.toInt) != 0 }
        readAndResolveUtf8Constant(bytes, offset + 2, pool).andThen { methodName =>
          readAndResolveUtf8Constant(bytes, offset + 4, pool).andThen { descriptor =>
            readAttributes(bytes, offset + 6, pool).map { (attributes, nextOffset) =>
              (MethodInfo(accessFlags.toSet, methodName, descriptor, attributes), nextOffset)
            }
          }
        }
      }
  }

  private[classparser] def readMethods(bytes: Array[Byte], offset: Int, pool: ConstantPool): Result[(List[MethodInfo], NextOffset)] = {

    read2Bytes(bytes, offset, "method count").andThen { methodCount =>
      (0 until methodCount.toInt).toList.foldLeft((List.empty[MethodInfo], offset + 2).validNel[String]) {
        case (accResult, _) =>
          accResult.andThen { case (methods, currentOffset) =>
            readMethod(bytes, currentOffset, pool).map { case (method, nextOffset) =>
              (methods :+ method, nextOffset)
            }
          }
      }
    }
  }


  //  def load(path: String): Result[ClassFileInfo] = {
  //    Validated.catchNonFatal(Files.readAllBytes(Paths.get(path)))
  //      .leftMap(e => NonEmptyList.of(s"Failed to open $path: $e"))
  //      .andThen(bytes => fromBytes(bytes))
  //  }

  def fromBytes(bytes: Array[Byte]): Result[ClassFileInfo] = {
    Validated.condNel(magicNumberMatch(bytes), bytes, s"class bytes do not start with a magic number bruh")
      .andThen(bytes =>
        (majorVersion(bytes), minorVersion(bytes), getPoolsize(bytes)).mapN { (major, minor, poolSize) =>
          (bytes, (major, minor, poolSize))
        }
      )
      .andThen { case (bytes, (major, minor, poolSize)) =>
        readAllPoolEntry(bytes, poolSize.toInt).map { (pool, nextOffset) =>
          (bytes, minor.toInt, major.toInt, pool, nextOffset)
        }
      }
      .andThen { case (bytes, minor, major, pool, nextOffset) =>
        readProperties(bytes, nextOffset, pool).andThen { (properties, propertiesNextOffset) =>
          readFields(bytes, propertiesNextOffset, pool).andThen { (fields, fieldsNextOffset) =>
            readMethods(bytes, fieldsNextOffset, pool).andThen { (methods, methodsNextOffset) =>
              val methodByName = methods.groupBy(x => (x.name.value, x.descriptor.value)).view.mapValues(_.head).toMap
              readAttributes(bytes, methodsNextOffset, pool).map { (attributes, _) =>
                ClassFileInfo(minor, major, pool, properties, fields, methodByName, attributes)
              }
            }
          }
        }
      }
  }

  private[classparser] type MethodName = String
  private[classparser] type MethodDescriptorStr = String
}

class ClassFileInfo(minorVersion: Int,
                    majorVersion: Int,
                    constantPool: ConstantPool,
                    classFileProperties: ClassFileProperties,
                    fields: List[ClassFileInfo.FieldInfo],
                    methods: Map[(MethodName, MethodDescriptorStr), ClassFileInfo.MethodInfo],
                    classFileAttributes: List[ClassFileInfo.Attribute]) {
  def initClass(): Clazz = {
    val (staticFieldsRaw, instanceFieldsRaw) = fields.partition(_.accessFlags.contains(ACC_STATIC))
    val staticFields = staticFieldsRaw.map { x =>
      ((x.name.value, x.descriptor.value), FValue.initFromDescriptor(x.descriptor.value))
    }.toMap
    val instanceFields = instanceFieldsRaw.map { f => (f.name.value, f.descriptor.value) }
    Clazz(classFileProperties.thisClass.name.value, mutable.Map.from(staticFields), toRuntimeConstantPool, toRuntimeJvmMethod, toNativeMethod, instanceFields)
  }

  private def toRuntimeJvmMethod: Map[(MethodName, MethodDescriptor), JvmMethod] = {
    methods.collect {
      case m if !m._2.isNative =>
        val code = m._2.code.get
        val methodName = m._1._1
        val methodDescriptor = MethodDescriptor.parseMethodDescriptor(m._1._2)
        (methodName, methodDescriptor) -> JvmMethod(methodName, methodDescriptor, code.maxStack, code.maxLocals, code.code, code.exceptionTable.map(toRuntimeExceptionTableEntry))
    }
  }

  private def toNativeMethod: Map[(MethodName, MethodDescriptor), NativeMethod] = {
    methods.collect {
      case m if m._2.isNative =>
        val methodName = m._1._1
        val methodDescriptor = MethodDescriptor.parseMethodDescriptor(m._1._2)
        (methodName, methodDescriptor) -> NativeMethod(methodName, methodDescriptor)
    }
  }

  private def toRuntimeExceptionTableEntry(e: ClassFileInfo.ExceptionTableEntry): RuntimeExceptionTableEntry =
    RuntimeExceptionTableEntry(e.startPc, e.endPc, e.handlerPc, e.catchType.map(toRuntimeClass))

  private def toRuntimeConstantPool: RuntimeConstantPool = {
    val entries = constantPool.toVector.map(_.map(toRuntimePoolEntry)).toArray
    RuntimeConstantPool(entries)
  }

  private def toRuntimePoolEntry(entry: ClassFileInfo.PoolEntry): RuntimePoolEntry = entry match {
    case ClassFileInfo.PoolEntry.CONSTANT_Class_info(name) =>
      RuntimePoolEntry.CONSTANT_Class_info(toRuntimeUtf8(name), None)
    case ClassFileInfo.PoolEntry.CONSTANT_Fieldref_info(clazz, nameAndType) =>
      RuntimePoolEntry.CONSTANT_Fieldref_info(toRuntimeClass(clazz), toRuntimeNameAndType(nameAndType))
    case ClassFileInfo.PoolEntry.CONSTANT_Methodref_info(clazz, nameAndType) =>
      RuntimePoolEntry.CONSTANT_Methodref_info(toRuntimeClass(clazz), toRuntimeNameAndType(nameAndType), None)
    case ClassFileInfo.PoolEntry.CONSTANT_InterfaceMethodref_info(clazz, nameAndType) =>
      RuntimePoolEntry.CONSTANT_InterfaceMethodref_info(toRuntimeClass(clazz), toRuntimeNameAndType(nameAndType))
    case ClassFileInfo.PoolEntry.CONSTANT_String_info(string) =>
      RuntimePoolEntry.CONSTANT_String_info(toRuntimeUtf8(string))
    case ClassFileInfo.PoolEntry.CONSTANT_Integer_info(value) => RuntimePoolEntry.CONSTANT_Integer_info(value)
    case ClassFileInfo.PoolEntry.CONSTANT_Float_info(value) => RuntimePoolEntry.CONSTANT_Float_info(value)
    case ClassFileInfo.PoolEntry.CONSTANT_Long_info(value) => RuntimePoolEntry.CONSTANT_Long_info(value)
    case ClassFileInfo.PoolEntry.CONSTANT_Double_info(value) => RuntimePoolEntry.CONSTANT_Double_info(value)
    case ClassFileInfo.PoolEntry.CONSTANT_NameAndType_info(name, descriptor) =>
      toRuntimeNameAndType(ClassFileInfo.PoolEntry.CONSTANT_NameAndType_info(name, descriptor))
    case ClassFileInfo.PoolEntry.CONSTANT_Utf8_info(value) => RuntimePoolEntry.CONSTANT_Utf8_info(value)
    case ClassFileInfo.PoolEntry.CONSTANT_MethodHandle_info(referenceKind, reference) =>
      RuntimePoolEntry.CONSTANT_MethodHandle_info(referenceKind, toRuntimePoolEntry(reference))
    case ClassFileInfo.PoolEntry.CONSTANT_MethodType_info(descriptor) =>
      RuntimePoolEntry.CONSTANT_MethodType_info(toRuntimeUtf8(descriptor))
    case ClassFileInfo.PoolEntry.CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex, nameAndType) =>
      RuntimePoolEntry.CONSTANT_InvokeDynamic_info(bootstrapMethodAttrIndex, toRuntimeNameAndType(nameAndType))
  }

  private def toRuntimeUtf8(u: ClassFileInfo.PoolEntry.CONSTANT_Utf8_info): RuntimePoolEntry.CONSTANT_Utf8_info =
    RuntimePoolEntry.CONSTANT_Utf8_info(u.value)

  private def toRuntimeClass(c: ClassFileInfo.PoolEntry.CONSTANT_Class_info): RuntimePoolEntry.CONSTANT_Class_info =
    RuntimePoolEntry.CONSTANT_Class_info(toRuntimeUtf8(c.name), None)

  private def toRuntimeNameAndType(n: ClassFileInfo.PoolEntry.CONSTANT_NameAndType_info): RuntimePoolEntry.CONSTANT_NameAndType_info =
    RuntimePoolEntry.CONSTANT_NameAndType_info(toRuntimeUtf8(n.name), toRuntimeUtf8(n.descriptor))

  override def toString: String = {
    val thisClassName = classFileProperties.thisClass.name.value
    val superClassName = classFileProperties.superClass.fold("<none>")(_.name.value)
    val flags = classFileProperties.flags.mkString(", ")
    val interfaces = classFileProperties.interfaces.map(_.name.value).mkString(", ")
    val pool = constantPool.toVector.zipWithIndex.collect { case (Some(entry), idx) => s"    #$idx = $entry" }.mkString("\n")
    val fieldsStr = fields.map { f =>
      val attrs = if f.attributes.isEmpty then "" else s" ${f.attributes.mkString(", ")}"
      s"    ${f.accessFlags.mkString(", ")} ${f.descriptor.value} ${f.name.value}$attrs"
    }.mkString("\n")
    val methodsStr = methods.values.map { m =>
      val attrs = if m.attributes.isEmpty then "" else s" ${m.attributes.mkString(", ")}"
      s"    ${m.accessFlags.mkString(", ")} ${m.name.value}${m.descriptor.value}$attrs"
    }.mkString("\n")
    val attributesStr = classFileAttributes.map(a => s"    $a").mkString("\n")
    s"""ClassFile($thisClassName extends $superClassName)
       |  majorVersion=$majorVersion
       |  minorVersion=$minorVersion
       |  constantPoolCount=${constantPool.size}
       |  flags=[$flags]
       |  interfaces=[$interfaces]
       |  constantPool=
       |$pool
       |  fields=
       |$fieldsStr
       |  methods=
       |$methodsStr
       |  attributes=
       |$attributesStr""".stripMargin
  }
}
