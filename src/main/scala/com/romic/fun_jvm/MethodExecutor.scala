package com.romic.fun_jvm

import com.romic.fun_jvm.FValue.FValueInt
import com.romic.fun_jvm.BytecodeExecutor.!!!
import com.romic.fun_jvm.RuntimeConstantPool.LongOrDouble.{Double_, Long_}
import com.romic.fun_jvm.RuntimeConstantPool.MethodRef
import com.romic.fun_jvm.utils.Utils.{UByte, UShort, to, toInt0Ext, toUShort}

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

  def size: Int = stack.size

  def mkString(start: String, sep: String, end: String): String = stack.mkString(start, sep, end)
}

object Frame {

  def apply(maxLocals: Int, currentClass: Clazz, pc: Int = 0): Frame =
    new Frame(Array.fill(maxLocals)(null), OperandStack(), currentClass, pc)

}

class Frame(
  val localVariables: LocalVariables,
  val operandStack: OperandStack,
  val currentClass: Clazz,
  var pc: Int = 0
) {

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

class MethodExecutorFactory(nativeMethodCatalog: NativeMethodCatalog, objectClazz: Clazz) {

  def generateExecutorForMethod(
    method: NativeMethod | JvmMethod,
    clazz: Clazz,
    threadState: FThreadState
  ): MethodExecutor = {
    val thread = threadState.thread
    val frame = threadState.thread.stack.head
    val operandStack = frame.operandStack
    val localVariables = frame.localVariables
    val currentClazz = frame.currentClass
    val constantPool = currentClazz.constantPool
    val classLoader = threadState.classLoader
    val heap = threadState.heap
    println(operandStack.size)

    def invokeJvmMethod(jvmMethod: JvmMethod, clazz: Clazz): BytecodeExecutor = {
      val newFrame = Frame(jvmMethod.maxLocals.toInt, clazz)
      val params = jvmMethod.methodDescriptor.params.indices.map(_ => operandStack.pop()).reverse
      val ref = operandStack.pop()
      newFrame.localVariables(0) = ref
      params.zipWithIndex.foreach((value, idx) => newFrame.localVariables(idx + 1) = value)
      thread.stack.push(newFrame)
      new BytecodeExecutor(thread, jvmMethod, classLoader, heap, objectClazz, this)
    }

    def invokeNativeMethod(nativeMethod: NativeMethod, clazz: Clazz): NativeExecutor = {
      val params = method.methodDescriptor.params.iterator.map(_ => operandStack.pop()).toList.reverse
      val newFrame = Frame(0, clazz)
      thread.stack.push(newFrame)
      new NativeExecutor(thread, nativeMethod, classLoader, heap, objectClazz, nativeMethodCatalog, this, params)
    }

    method match {
      case nativeMethod: NativeMethod => invokeNativeMethod(nativeMethod, clazz)
      case jvmMethod: JvmMethod => invokeJvmMethod(jvmMethod, clazz)
    }
  }

}

trait MethodExecutor {
  def run(): Unit

  def thread: FThread

  def classLoader: FClassLoader

  def heap: Heap

  protected def getThreadState: FThreadState = FThreadState(heap, classLoader, thread)

}

class NativeExecutor(
  val thread: FThread,
  method: NativeMethod,
  val classLoader: FClassLoader,
  val heap: Heap,
  objectClazz: Clazz,
  nativeMethodCatalog: NativeMethodCatalog,
  executorFactory: MethodExecutorFactory,
  params: List[FValue]
) extends MethodExecutor {
  private val frame = thread.stack.head
  private val operandStack = frame.operandStack
  private val localVariables = frame.localVariables
  private val currentClazz = frame.currentClass
  private val constantPool = currentClazz.constantPool

  def run(): Unit = {
    val meth = nativeMethodCatalog.get(currentClazz, method)
    meth(getThreadState, executorFactory, params)
  }

}

object BytecodeExecutor {

  def apply(
    method: JvmMethod,
    clazz: Clazz,
    classLoader: FClassLoader,
    heap: Heap,
    objectClazz: Clazz,
    nativeMethodCatalog: NativeMethodCatalog
  ): BytecodeExecutor = {
    val thread = FThread(0, method.maxLocals.toInt, clazz)
    val factory = new MethodExecutorFactory(nativeMethodCatalog, objectClazz)
    new BytecodeExecutor(thread, method, classLoader, heap, objectClazz, factory)
  }

  def !!! : Nothing = throw new RuntimeException("UnexpectedType")
}

class BytecodeExecutor(
  val thread: FThread,
  method: JvmMethod,
  val classLoader: FClassLoader,
  val heap: Heap,
  objectClazz: Clazz,
  executorFactory: MethodExecutorFactory
) extends MethodExecutor {
  private val frame = thread.stack.head
  private val operandStack = frame.operandStack
  private val localVariables = frame.localVariables
  private val currentClazz = frame.currentClass
  private val constantPool = currentClazz.constantPool
  private val code = method.code

  private def invokeMethod(method: NativeMethod | JvmMethod, clazz: Clazz): MethodExecutor =
    executorFactory.generateExecutorForMethod(method, clazz, getThreadState)

  private def readNext: Byte = {
    frame.pc += 1
    code(frame.pc)
  }

  private def readNextUnsigned: UByte = {
    frame.pc += 1
    UByte(code(frame.pc))
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
    println(s"================== NOW RUNNING ${currentClazz.name}.${method.methodName} ${method.methodDescriptor}")
    println(s"0x ${code.map(_.toInt0Ext.toHexString).mkString(" 0x")}")
    println(OpCode.codesToString(code))

    @tailrec
    def runLoop(): Unit = {
      if (frame.pc < code.size) {
        println(s"RUNNING OP ${OpCode.nameOf(code(frame.pc))} 0x${code(frame.pc).toInt0Ext.toHexString}")
        code(frame.pc) match {
          case x if (OpCode.iconst_m1.op to OpCode.iconst_5.op).contains(x) => iconst_i(x - OpCode.iconst_0.op)
          case x if (OpCode.istore_0.op to OpCode.istore_3.op).contains(x) => istore_n(x - OpCode.istore_0.op)
          case x if (OpCode.iload_0.op to OpCode.iload_3.op).contains(x) => iload_n(x - OpCode.iload_0.op)
          case OpCode.bipush.op => bipush(readNext)
          case OpCode.putstatic.op => putstatic(readNext, readNext)
          case OpCode.aload.op => aload(readNextUnsigned)
          case x if (OpCode.aload_0.op to OpCode.aload_3.op).contains(x) => aload_n(x - OpCode.aload_0.op)
          case OpCode.return_.op =>
            println("================== RETURN")
            thread.stack.pop()
            return
          case OpCode.getstatic.op => getstatic(readNext, readNext)
          case OpCode.invokestatic.op => invokestatic(readNext, readNext)
          case OpCode.iadd.op => iadd()
          case OpCode.aconst_null.op => aconst_null
          case OpCode.ldc2_w.op => ldc2_w(readNext, readNext)
          case OpCode.anewarray.op => anewarray(readNext, readNext)
          case OpCode.invokespecial.op => invokespecial(readNext, readNext)
          case OpCode.ldc.op => ldc(readNext)
          case OpCode.new_.op => new_(readNext, readNext)
          case OpCode.newarray.op => newarray(readNext)
          case OpCode.dup.op => dup()
          case OpCode.castore.op => castore()
          case uh =>
            debug()
            throw new RuntimeException(s"watdat opcode ? 0x${(uh & 0xff).toHexString} / ${uh}")
        }
        frame.pc += 1
        runLoop()
      }
    }

    runLoop()
  }

  private def aload(index: UByte): Unit = {
    localVariables(index.toInt) match {
      case r: FValue.FValueReference => operandStack.push(r)
      case wut => throw new RuntimeException(s"aload was expecting ref, got $wut")
    }
  }

  private def aload_n(n: Int): Unit =
    aload(UByte(n))

  private def castore(): Unit = {
    val value = popInt()
    val index = popInt()
    val arrayRef = popRef()
    heap.writeBytes2Arr(arrayRef.toHeapAddr, index.value, value.toBytes)
  }

  private def dup(): Unit =
    operandStack.push(operandStack.head)

  private def new_(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val clazz = constantPool.resolveClass(idx).resolveClazz(classLoader)
    operandStack.push(heap.storeNew(clazz).toRef)
  }

  private def aconst_null =
    operandStack.push(FValue.FValueReference.null_)

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

    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val methodRef = constantPool.resolveMethodRef(idx)
    val (clazz, method) = methodRef match {
      case MethodRef.InstanceMethod(v) => v.resolveMethod(classLoader)
      case MethodRef.InterfaceMethod(v) => v.resolveMethod(classLoader)
    }
    method match {
      case j: JvmMethod =>
        invokeMethod(j, clazz).run()
      case nmeth: NativeMethod =>
        invokeMethod(nmeth, clazz).run()
      case com.romic.fun_jvm.AbstractMethod(_, _, _) => ???
    }
  }

  private def invokespecial(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val (clazz, (fname, fdesc)) = constantPool.resolveMethodRef(idx) match {
      case MethodRef.InstanceMethod(v) =>
        val resolvedClazz = v.clazz.resolveClazz(classLoader)
        if (v.nameAndType.name.value != "<init>" && currentClazz.isASuperClassOfThis(resolvedClazz)) {
          (currentClazz.maybeDirectSuperClass.get, v.nameAndType.toTuple)
        } else {
          (resolvedClazz, v.nameAndType.toTuple)
        }
      case MethodRef.InterfaceMethod(v) => (v.clazz.resolveClazz(classLoader), v.nameAndType.toTuple)
    }
    clazz.resolveFunctionRecurs(fname, fdesc, objectClazz) match {
      case j: JvmMethod => invokeMethod(j, clazz).run()
      case com.romic.fun_jvm.NativeMethod(_, _, _) => ???
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

  private def iadd(): Unit =
    operandStack.push(popInt() + popInt())

  private def bipush(value: Byte): Unit =
    operandStack.push(FValueInt(value.toInt))

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

  private def iconst_i(i: Int): Unit =
    operandStack.push(FValueInt(i))

  private def iload_n(n: Int): Unit =
    operandStack.push(localVariables(n))

  private def istore_n(n: Int): Unit = {
    val i = operandStack.pop() match {
      case i: FValueInt => i
      case _ => !!!
    }
    localVariables(n) = i
  }

  def debug(): Unit =
    println(debugStr())

  def debugStr(): String = {
    val currentOp = if (frame.pc < code.size) f"0x${code(frame.pc) & 0xff}%02x" else "<end of code>"
    s"""================== DEBUG ${currentClazz.name}.${method.methodName}${method.methodDescriptor}
       |pc: ${frame.pc}
       |current opcode: $currentOp ${OpCode.codeToString(code.drop(frame.pc))}
       |call stack depth: ${thread.stack.size}
       |${
        thread.stack.zipWithIndex.map { case (f, i) =>
          s"--- frame $i${if (i == 0) " (current)" else ""} ---\n${f.debug}"
        }.mkString("\n")
      }
       |${heap.debug}
       |${constantPool.debug}
       |""".stripMargin
  }

}
