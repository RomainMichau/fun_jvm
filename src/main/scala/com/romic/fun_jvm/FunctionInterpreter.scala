package com.romic.fun_jvm

import FValue.{FValueByte, FValueInt}
import FunctionInterpreter.!!!
import com.romic.fun_jvm.utils.Utils.{UInt, UShort, to, toInt0Ext, toUShort}
import RuntimeConstantPool.LongOrDouble.Double_
import RuntimeConstantPool.LongOrDouble.Long_
import com.romic.fun_jvm.Clazz.MethodId

import scala.annotation.tailrec
import scala.collection.mutable

type LocalVariables = Array[FValue]
type operand = Byte


object OperandStack {
  def apply(): OperandStack = new OperandStack
}

class OperandStack {
  private val stack: mutable.Stack[FValue] = mutable.Stack.empty

  def push(v: FValue): Unit = stack.push(v)

  def pop(): FValue = stack.pop()

  def head: FValue = stack.head

  def drop(n: Int): Unit = stack.drop(n)

  def mkString(start: String, sep: String, end: String): String = stack.mkString(start, sep, end)
}

object Frame {
  def apply(maxLocals: Int): Frame = new Frame(Array.fill(maxLocals)(null), OperandStack())
}

class Frame(val localVariables: LocalVariables = Array.empty, val operandStack: OperandStack) {
  def debug: String = {
    val locals = localVariables.zipWithIndex
      .map { case (v, i) => s"  [$i] = $v" }
      .mkString("\n")
    val stack = operandStack.mkString("\n  ", "\n  ", "")
    s"""Frame(
       |localVariables:
       |$locals
       |operandStack:$stack
       |)""".stripMargin
  }
}

object FThread {
  def apply(pc: Int = 0, maxLocals: Int): FThread = new FThread(pc, mutable.Stack(Frame(maxLocals)))

}

class FThread(var pc: Int = 0, val stack: mutable.Stack[Frame])

object FunctionInterpreter {
  def apply(method: JvmMethod, clazz: Clazz, classLoader: ClassLoader, heap: Heap): FunctionInterpreter = {
    val thread = FThread(0, method.maxLocals.toInt)
    new FunctionInterpreter(thread, method, clazz, classLoader, heap)
  }

  def !!! : Nothing = throw new RuntimeException("UnexpectedType")
}

class FunctionInterpreter(thread: FThread, method: JvmMethod, clazz: Clazz, classLoader: ClassLoader, heap: Heap) {
  private val frame = thread.stack.head
  private val operandStack = thread.stack.head.operandStack
  private val localVariables = thread.stack.head.localVariables
  private val constantPool = clazz.constantPool
  private val code = method.code


  private def readNext: Byte = {
    thread.pc += 1
    code(thread.pc)
  }

  private def popInt(): FValueInt = {
    operandStack.pop() match {
      case i: FValueInt => i
      case bru => throw new RuntimeException(s"was expeting to pop int, got $bru, wtf bro")
    }
  }

  private def popRef(): FValue.FValueReference = {
    operandStack.pop() match {
      case i: FValue.FValueReference => i
      case bru => throw new RuntimeException(s"was expeting to pop ref, got $bru, wtf bro")
    }
  }

  def run(): Unit = {
    //    println(constantPool.toVector.zipWithIndex.map(x => (x._2, x._1)).mkString("\n"))
    println(s"================== NOW RUNNING ${clazz.name}.${method.methodName} ${method.methodDescriptor}")
    println(s"0x ${code.map(_.toInt0Ext.toHexString).mkString(" 0x")}")
    println(OpCode.codesToString(code))

    @tailrec
    def runLoop(): Unit = {
      if (thread.pc < code.size) {
        println(s"RUNNING OP ${OpCode.nameOf(code(thread.pc))} 0x${code(thread.pc).toInt0Ext.toHexString}")
        code(thread.pc) match {
          case x if (OpCode.iconst_m1.op to OpCode.iconst_5.op).contains(x) => iconst_i(x - OpCode.iconst_0.op)
          case x if (OpCode.istore_0.op to OpCode.istore_3.op).contains(x) => istore_n(x - OpCode.istore_0.op)
          case x if (OpCode.iload_0.op to OpCode.iload_3.op).contains(x) => iload_n(x - OpCode.iload_0.op)
          case OpCode.bipush.op => bipush(readNext)
          case OpCode.putstatic.op => putstatic(readNext, readNext)
          case OpCode.return_.op =>
            println("================== RETURN")
            return
          case OpCode.getstatic.op => getstatic(readNext, readNext)
          case OpCode.invokestatic.op => invokestatic(readNext, readNext)
          case OpCode.iadd.op => iadd()
          case OpCode.aconst_null.op => aconst_null
          case OpCode.ldc2_w.op => ldc2_w(readNext, readNext)
          case OpCode.anewarray.op => anewarray(readNext, readNext)
          case OpCode.ldc.op => ldc(readNext)
          case OpCode.new_.op => new_(readNext, readNext)
          case OpCode.newarray.op => newarray(readNext)
          case OpCode.dup.op => dup()
          case OpCode.castore.op => castore()
          case uh =>
            debug()
            throw new RuntimeException(s"watdat opcode ? 0x${(uh & 0xff).toHexString} / ${uh}")
        }
        thread.pc += 1
        runLoop()
      }
    }

    runLoop()
  }

  private def castore(): Unit = {
    val value = popInt()
    val index = popInt()
    val arrayRef = popRef()
    heap.writeBytes2Arr(arrayRef.toHeapAddr, index.value, value.toBytes)
  }

  private def dup(): Unit = {
    operandStack.push(operandStack.head)
  }

  private def new_(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val clazz = constantPool.resolveClass(idx).resolveClazz(classLoader)
    operandStack.push(heap.storeNew(clazz).toRef)
  }


  private def aconst_null = {
    operandStack.push(FValue.FValueReference.null_)
  }

  private def newarray(atype: Int): Unit = {
    val arraytype = atype match {
      case 4 => FType.FTypeBoolean
      case 5 => FType.FTypeChar
      case 6 => FType.FTypeFloat
      case 7 => FType.FTypeDouble
      case 8 => FType.FTypeByte
      case 9 => FType.FTypeShort
      case 10 => FType.FTypeInt
      case 11 => FType.FTypeLong
    }
    val ref = heap.allocateArray(arraytype, popInt().value)
    operandStack.push(ref.toRef)
  }

  private def anewarray(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val classRef = constantPool.resolveClass(idx).resolveClazz(classLoader)
    val type_ = FType.FTypeReference(classRef.name)
    val ref = heap.allocateArray(type_, popInt().value).toRef
    operandStack.push(ref)
  }

  private def invokestatic(index1: Byte, index2: Byte): Unit = {
    def invokeStaticNative(clazz: Clazz, method: NativeMethod): Unit = {
      operandStack.drop(method.methodDescriptor.param.length)
      NativeMethodCatalog.get(clazz, method)()
    }

    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val methodRef = constantPool.resolveMethodRef(idx)
    val (clazz, method) = methodRef.resolveMethod(classLoader)
    method match {
      case JvmMethod(methodName, methodDescriptor, maxStack, maxLocals, code, exceptionTable) => ???
      case nmeth: NativeMethod => invokeStaticNative(clazz, nmeth)
    }
  }

  private def putstatic(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val field = constantPool.resolveFieldref(idx)
    val clazz = field.clazz.resolveClazz(classLoader)
    clazz.staticFields(field.nameAndType.toTupleSt) = operandStack.pop()
  }

  private def getstatic(index1: Byte, index2: Byte) = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val field = constantPool.resolveFieldref(idx)
    val clazz = field.clazz.resolveClazz(classLoader)
    val value = clazz.staticFields((field.nameAndType.name.value, field.nameAndType.descriptor.value))
    operandStack.push(value)
  }

  private def iadd(): Unit = {
    operandStack.push(popInt() + popInt())
  }

  private def bipush(value: Byte): Unit = {
    operandStack.push(FValueInt(value.toInt))
  }

  private def ldc(idx: Byte) = {
    val toPush = constantPool.resolveIntFlotOrRef(idx.toUShort) match {
      case RuntimeConstantPool.IntFloatOrRef.Float_(d) => FValue.FValueDouble(d)
      case RuntimeConstantPool.IntFloatOrRef.Int_(i) => FValue.FValueDouble(i)
      case RuntimeConstantPool.IntFloatOrRef.ClassRef(c) =>
        c.resolveClazz(classLoader).getClassMirror.toRef
      case RuntimeConstantPool.IntFloatOrRef.StringRef(s) =>
        s.resolveRef(heap).toRef
      case s => throw new RuntimeException(s"ldc was not expecting $s")
    }
    operandStack.push(toPush)
  }

  private def ldc2_w(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val toPush = constantPool.resolveLongOrDouble(idx) match {
      case Double_(d) => FValue.FValueDouble(d)
      case Long_(l) => FValue.FValueLong(l)
    }
    operandStack.push(toPush)
  }

  private def iconst_i(i: Int): Unit = {
    operandStack.push(FValueInt(i))
  }

  private def iload_n(n: Int): Unit = {
    operandStack.push(localVariables(n))
  }

  private def istore_n(n: Int): Unit = {
    val i = operandStack.pop() match {
      case i: FValueInt => i
      case _ => !!!
    }
    localVariables(n) = i
  }

  def debug(): Unit = {
    println(debugStr())
  }

  def debugStr(): String = {
    val currentOp = if (thread.pc < code.size) f"0x${code(thread.pc) & 0xff}%02x" else "<end of code>"
    s"""================== DEBUG ${clazz.name}.${method.methodName}${method.methodDescriptor}
       |pc: ${thread.pc}
       |current opcode: $currentOp ${OpCode.codeToString(code.drop(thread.pc))}
       |call stack depth: ${thread.stack.size}
       |${thread.stack.zipWithIndex.map { case (f, i) => s"--- frame $i${if (i == 0) " (current)" else ""} ---\n${f.debug}" }.mkString("\n")}
       |${heap.debug}
       |${constantPool.debug}
       |""".stripMargin
  }
}