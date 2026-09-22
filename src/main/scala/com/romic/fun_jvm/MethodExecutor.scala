package com.romic.fun_jvm

import com.romic.fun_jvm.FValue.FValueInt
import com.romic.fun_jvm.BytecodeExecutor.!!!
import com.romic.fun_jvm.RuntimeConstantPool.LongOrDouble.{Double_, Long_}
import com.romic.fun_jvm.RuntimeConstantPool.MethodRef
import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.utils.Utils.{UByte, UShort, to, toInt0Ext, toUShort}
import com.romic.fun_jvm.well_known.WKString

import scala.annotation.tailrec
import scala.collection.mutable

//type LocalVariables = Array[FValue]
type operand = Byte

object LocalVariables {
  def apply(maxCount: Int) = new LocalVariables(maxCount)
}

class LocalVariables(maxSize: Int) {
  private val array: Array[FValue] = Array.fill(maxSize)(null)

  def update(idx: Int, v: FValue): Unit = array(idx) = v

  def apply(idx: Int): FValue = array(idx)

  def zipWithIndex: Array[(FValue, Int)] = array.zipWithIndex

  // Writes values sequentially starting at startIdx, advancing 2 slots for long/double
  // (category 2 types) and 1 slot for everything else, matching real local variable indexing.
  def setAll(startIdx: Int, values: Iterable[FValue]): Unit = {
    var idx = startIdx
    values.foreach { v =>
      array(idx) = v
      idx += (if (v.getType.byteCount == 8) 2 else 1)
    }
  }

  def getInt(idx: Int): FValue.FValueInt = array(idx) match {
    case v: FValue.FValueInt => v
    case v => throw new RuntimeException(s"expected FValueInt at local $idx, got $v")
  }

  def getLong(idx: Int): FValue.FValueLong = array(idx) match {
    case v: FValue.FValueLong => v
    case v => throw new RuntimeException(s"expected FValueLong at local $idx, got $v")
  }

  def getShort(idx: Int): FValue.FValueShort = array(idx) match {
    case v: FValue.FValueShort => v
    case v => throw new RuntimeException(s"expected FValueShort at local $idx, got $v")
  }

  def getByte(idx: Int): FValue.FValueByte = array(idx) match {
    case v: FValue.FValueByte => v
    case v => throw new RuntimeException(s"expected FValueByte at local $idx, got $v")
  }

  def getFloat(idx: Int): FValue.FValueFloat = array(idx) match {
    case v: FValue.FValueFloat => v
    case v => throw new RuntimeException(s"expected FValueFloat at local $idx, got $v")
  }

  def getDouble(idx: Int): FValue.FValueDouble = array(idx) match {
    case v: FValue.FValueDouble => v
    case v => throw new RuntimeException(s"expected FValueDouble at local $idx, got $v")
  }

  def getRef(idx: Int): FValue.FValueReference = array(idx) match {
    case v: FValue.FValueReference => v
    case v => throw new RuntimeException(s"expected FValueReference at local $idx, got $v")
  }

  def getChar(idx: Int): FValue.FValueChar = array(idx) match {
    case v: FValue.FValueChar => v
    case v => throw new RuntimeException(s"expected FValueChar at local $idx, got $v")
  }

  def getBoolean(idx: Int): FValue.FValueBoolean = array(idx) match {
    case v: FValue.FValueBoolean => v
    case v => throw new RuntimeException(s"expected FValueBoolean at local $idx, got $v")
  }

  def getArray(idx: Int): FValue.FValueArray[?] = array(idx) match {
    case v: FValue.FValueArray[?] => v
    case v => throw new RuntimeException(s"expected FValueArray at local $idx, got $v")
  }

}

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

  def popInt(): FValue.FValueInt = stack.pop() match {
    case v: FValue.FValueInt => v
    case v => throw new RuntimeException(s"expected FValueInt on operand stack, got $v")
  }

  def popLong(): FValue.FValueLong = stack.pop() match {
    case v: FValue.FValueLong => v
    case v => throw new RuntimeException(s"expected FValueLong on operand stack, got $v")
  }

  def popShort(): FValue.FValueShort = stack.pop() match {
    case v: FValue.FValueShort => v
    case v => throw new RuntimeException(s"expected FValueShort on operand stack, got $v")
  }

  def popByte(): FValue.FValueByte = stack.pop() match {
    case v: FValue.FValueByte => v
    case v => throw new RuntimeException(s"expected FValueByte on operand stack, got $v")
  }

  def popFloat(): FValue.FValueFloat = stack.pop() match {
    case v: FValue.FValueFloat => v
    case v => throw new RuntimeException(s"expected FValueFloat on operand stack, got $v")
  }

  def popDouble(): FValue.FValueDouble = stack.pop() match {
    case v: FValue.FValueDouble => v
    case v => throw new RuntimeException(s"expected FValueDouble on operand stack, got $v")
  }

  def popRef(): FValue.FValueReference = stack.pop() match {
    case v: FValue.FValueReference => v
    case v => throw new RuntimeException(s"expected FValueReference on operand stack, got $v")
  }

  def popChar(): FValue.FValueChar = stack.pop() match {
    case v: FValue.FValueChar => v
    case v => throw new RuntimeException(s"expected FValueChar on operand stack, got $v")
  }

  def popBoolean(): FValue.FValueBoolean = stack.pop() match {
    case v: FValue.FValueBoolean => v
    case v => throw new RuntimeException(s"expected FValueBoolean on operand stack, got $v")
  }

  def popArray(): FValue.FValueArray[?] = stack.pop() match {
    case v: FValue.FValueArray[?] => v
    case v => throw new RuntimeException(s"expected FValueArray on operand stack, got $v")
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
      newFrame.localVariables.setAll(1, params)
      new BytecodeExecutor(thread.push(newFrame), jvmMethod, classLoader, heap, objectClazz, this)
    }

    def invokeNativeMethod(nativeMethod: NativeMethod, clazz: Clazz): NativeExecutor = {
      val params = method.methodDescriptor.params.iterator.map(_ => operandStack.pop()).toList.reverse
      val newFrame = Frame(0, clazz)
      new NativeExecutor(
        thread.push(newFrame),
        nativeMethod,
        classLoader,
        heap,
        objectClazz,
        nativeMethodCatalog,
        this,
        params
      )
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
          // ===== Constants =====
          case OpCode.nop.op => nop()
          case OpCode.aconst_null.op => aconst_null
          case x if (OpCode.iconst_m1.op to OpCode.iconst_5.op).contains(x) => iconst_i(x - OpCode.iconst_0.op)
          case x if (OpCode.lconst_0.op to OpCode.lconst_1.op).contains(x) => lconst_n(x - OpCode.lconst_0.op)
          case x if (OpCode.fconst_0.op to OpCode.fconst_2.op).contains(x) => fconst((x - OpCode.fconst_0.op).toByte)
          case x if (OpCode.dconst_0.op to OpCode.dconst_1.op).contains(x) => dconst_n(x - OpCode.dconst_0.op)
          // ===== Push constant =====
          case OpCode.bipush.op => bipush(readNext)
          case OpCode.sipush.op => sipush(readNext, readNext)
          case OpCode.ldc.op => ldc(readNext)
          case OpCode.ldc_w.op => ldc_w(readNext, readNext)
          case OpCode.ldc2_w.op => ldc2_w(readNext, readNext)
          // ===== Loads =====
          case OpCode.iload.op => iload(readNext)
          case x if (OpCode.iload_0.op to OpCode.iload_3.op).contains(x) => iload_n(x - OpCode.iload_0.op)
          case OpCode.lload.op => lload(readNext)
          case x if (OpCode.lload_0.op to OpCode.lload_3.op).contains(x) => lload_n(x - OpCode.lload_0.op)
          case OpCode.fload.op => fload(readNext)
          case x if (OpCode.fload_0.op to OpCode.fload_3.op).contains(x) => fload_n((x - OpCode.fload_0.op).toByte)
          case OpCode.dload.op => dload(readNext)
          case x if (OpCode.dload_0.op to OpCode.dload_3.op).contains(x) => dload_n(x - OpCode.dload_0.op)
          case OpCode.aload.op => aload(readNextUnsigned)
          case x if (OpCode.aload_0.op to OpCode.aload_3.op).contains(x) => aload_n(x - OpCode.aload_0.op)
          // ===== Array loads =====
          case OpCode.iaload.op => iaload()
          case OpCode.laload.op => laload()
          case OpCode.faload.op => faload()
          case OpCode.daload.op => daload()
          case OpCode.aaload.op => aaload()
          case OpCode.baload.op => baload()
          case OpCode.caload.op => caload()
          case OpCode.saload.op => saload()
          // ===== Stores =====
          case OpCode.istore.op => istore(readNext)
          case x if (OpCode.istore_0.op to OpCode.istore_3.op).contains(x) => istore_n(x - OpCode.istore_0.op)
          case OpCode.lstore.op => lstore(readNext)
          case x if (OpCode.lstore_0.op to OpCode.lstore_3.op).contains(x) => lstore_n(x - OpCode.lstore_0.op)
          case OpCode.fstore.op => fstore(readNext)
          case x if (OpCode.fstore_0.op to OpCode.fstore_3.op).contains(x) => fstore_n(x - OpCode.fstore_0.op)
          case OpCode.dstore.op => dstore(readNext)
          case x if (OpCode.dstore_0.op to OpCode.dstore_3.op).contains(x) => dstore_n(x - OpCode.dstore_0.op)
          case OpCode.astore.op => astore(readNext)
          case x if (OpCode.astore_0.op to OpCode.astore_3.op).contains(x) => astore_n(x - OpCode.astore_0.op)
          // ===== Array stores =====
          case OpCode.iastore.op => iastore()
          case OpCode.lastore.op => lastore()
          case OpCode.fastore.op => fastore()
          case OpCode.dastore.op => dastore()
          case OpCode.aastore.op => aastore()
          case OpCode.bastore.op => bastore()
          case OpCode.castore.op => castore()
          case OpCode.sastore.op => sastore()
          // ===== Stack management =====
          case OpCode.pop.op => pop()
          case OpCode.pop2.op => pop2()
          case OpCode.dup.op => dup()
          case OpCode.dup_x1.op => dup_x1()
          case OpCode.dup_x2.op => dup_x2()
          case OpCode.dup2.op => dup2()
          case OpCode.dup2_x1.op => dup2_x1()
          case OpCode.dup2_x2.op => dup2_x2()
          case OpCode.swap.op => swap()
          // ===== Arithmetic =====
          case OpCode.iadd.op => iadd()
          case OpCode.ladd.op => ladd()
          case OpCode.fadd.op => fadd()
          case OpCode.dadd.op => dadd()
          case OpCode.isub.op => isub()
          case OpCode.lsub.op => lsub()
          case OpCode.fsub.op => fsub()
          case OpCode.dsub.op => dsub()
          case OpCode.imul.op => imul()
          case OpCode.lmul.op => lmul()
          case OpCode.fmul.op => fmul()
          case OpCode.dmul.op => dmul()
          case OpCode.idiv.op => idiv()
          case OpCode.ldiv.op => ldiv()
          case OpCode.fdiv.op => fdiv()
          case OpCode.ddiv.op => ddiv()
          case OpCode.irem.op => irem()
          case OpCode.lrem.op => lrem()
          case OpCode.frem.op => frem()
          case OpCode.drem.op => drem()
          case OpCode.ineg.op => ineg()
          case OpCode.lneg.op => lneg()
          case OpCode.fneg.op => fneg()
          case OpCode.dneg.op => dneg()
          case OpCode.ishl.op => ishl()
          case OpCode.lshl.op => lshl()
          case OpCode.ishr.op => ishr()
          case OpCode.lshr.op => lshr()
          case OpCode.iushr.op => iushr()
          case OpCode.lushr.op => lushr()
          case OpCode.iand.op => iand()
          case OpCode.land.op => land()
          case OpCode.ior.op => ior()
          case OpCode.lor.op => lor()
          case OpCode.ixor.op => ixor()
          case OpCode.lxor.op => lxor()
          case OpCode.iinc.op => iinc(readNext, readNext)
          // ===== Conversions =====
          case OpCode.i2l.op => i2l()
          case OpCode.i2f.op => i2f()
          case OpCode.i2d.op => i2d()
          case OpCode.l2i.op => l2i()
          case OpCode.l2f.op => l2f()
          case OpCode.l2d.op => l2d()
          case OpCode.f2i.op => f2i()
          case OpCode.f2l.op => f2l()
          case OpCode.f2d.op => f2d()
          case OpCode.d2i.op => d2i()
          case OpCode.d2l.op => d2l()
          case OpCode.d2f.op => d2f()
          case OpCode.i2b.op => i2b()
          case OpCode.i2c.op => i2c()
          case OpCode.i2s.op => i2s()
          // ===== Comparisons =====
          case OpCode.lcmp.op => lcmp()
          case OpCode.fcmpl.op => fcmpl()
          case OpCode.fcmpg.op => fcmpg()
          case OpCode.dcmpl.op => dcmpl()
          case OpCode.dcmpg.op => dcmpg()
          // ===== Control transfer =====
          case x if (OpCode.ifeq.op to OpCode.ifle.op).contains(x) =>
            ifcond((x - OpCode.ifeq.op).toByte, readNext, readNext)
          case x if (OpCode.if_icmpeq.op to OpCode.if_icmple.op).contains(x) =>
            if_icmp_cond((x - OpCode.if_icmpeq.op).toByte, readNext, readNext)
          case x if (OpCode.if_acmpeq.op to OpCode.if_acmpne.op).contains(x) =>
            if_acmp_cond((x - OpCode.if_acmpeq.op).toByte, readNext, readNext)
          case OpCode.goto.op => goto(readNext, readNext)
          case OpCode.jsr.op => jsr(readNext, readNext)
          case OpCode.ret.op => ret(readNext)
          // ===== Returns =====
          case x if (OpCode.ireturn.op to OpCode.areturn.op).contains(x) => typedReturn((x - OpCode.ireturn.op).toByte)
          case OpCode.return_.op =>
            println("================== RETURN")
            return
          // ===== Field access =====
          case OpCode.getstatic.op => getstatic(readNext, readNext)
          case OpCode.putstatic.op => putstatic(readNext, readNext)
          case OpCode.getfield.op => getfield(readNext, readNext)
          case OpCode.putfield.op => putfield(readNext, readNext)
          // ===== Method invocation =====
          case OpCode.invokevirtual.op => invokevirtual(readNext, readNext)
          case OpCode.invokespecial.op => invokespecial(readNext, readNext)
          case OpCode.invokestatic.op => invokestatic(readNext, readNext)
          case OpCode.invokeinterface.op => invokeinterface(readNext, readNext, readNext, readNext)
          case OpCode.invokedynamic.op => invokedynamic(readNext, readNext, readNext, readNext)
          // ===== Object/array creation and type checks =====
          case OpCode.new_.op => new_(readNext, readNext)
          case OpCode.newarray.op => newarray(readNext)
          case OpCode.anewarray.op => anewarray(readNext, readNext)
          case OpCode.arraylength.op => arraylength()
          case OpCode.athrow.op => athrow()
          case OpCode.checkcast.op => checkcast(readNext, readNext)
          case OpCode.instanceof.op => instanceof(readNext, readNext)
          // ===== Synchronization =====
          case OpCode.monitorenter.op => monitorenter()
          case OpCode.monitorexit.op => monitorexit()
          // ===== Extended (wide/tableswitch/lookupswitch skipped: variable-length operand encoding) =====
          case OpCode.multianewarray.op => multianewarray(readNext, readNext, readNext)
          case x if (OpCode.ifnull.op to OpCode.ifnonnull.op).contains(x) =>
            ifnull_cond((x - OpCode.ifnull.op).toByte, readNext, readNext)
          case OpCode.goto_w.op => goto_w(readNext, readNext, readNext, readNext)
          case OpCode.jsr_w.op => jsr_w(readNext, readNext, readNext, readNext)
          // ===== Reserved (never emitted by any legitimate compiler) =====
          case OpCode.breakpoint.op => breakpoint()
          case OpCode.impdep1.op => impdep1()
          case OpCode.impdep2.op => impdep2()
        }
        frame.pc += 1
        runLoop()
      }
    }

    runLoop()
  }

  // ===== Constants =====

  private def nop(): Unit = ()

  private def aconst_null =
    operandStack.push(FValue.FValueReference.null_)

  private def iconst_i(i: Int): Unit =
    operandStack.push(FValueInt(i))

  private def lconst_n(n: Int): Unit = throw new NotImplementedError("lconst_n not implemented")

  private def fconst(f: Byte): Unit = {
    f match {
      case 0 => operandStack.push(FValue.FValueFloat(0.0))
      case 1 => operandStack.push(FValue.FValueFloat(1.0))
      case 2 => operandStack.push(FValue.FValueFloat(2.0))
      case _ => throw new RuntimeException(s"Unexpected $f")
    }
  }

  private def dconst_n(n: Int): Unit = throw new NotImplementedError("dconst_n not implemented")

  // ===== Push constant =====

  private def bipush(value: Byte): Unit =
    operandStack.push(FValueInt(value.toInt))

  private def sipush(b1: Byte, b2: Byte): Unit = throw new NotImplementedError("sipush not implemented")

  private def ldc(idx: Byte) = {
    val toPush = constantPool.resolveIntFlotOrRef(idx.toUShort) match {
      case RuntimeConstantPool.IntFloatOrRef.Float_(d) => FValue.FValueFloat(d)
      case RuntimeConstantPool.IntFloatOrRef.Int_(i) => FValue.FValueDouble(i)
      case RuntimeConstantPool.IntFloatOrRef.ClassRef(c) =>
        c.resolveClazz(classLoader).getClassMirror(classLoader).toRef(c.name.value)
      case RuntimeConstantPool.IntFloatOrRef.StringRef(s) =>
        s.resolveRef(heap, classLoader).toRef(WKString.className)
      case s => throw new RuntimeException(s"ldc was not expecting $s")
    }
    operandStack.push(toPush)
  }

  private def ldc_w(b1: Byte, b2: Byte): Unit = throw new NotImplementedError("ldc_w not implemented")

  private def ldc2_w(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val toPush = constantPool.resolveLongOrDouble(idx) match {
      case Double_(d) => FValue.FValueDouble(d)
      case Long_(l) => FValue.FValueLong(l)
    }
    operandStack.push(toPush)
  }

  // ===== Loads =====

  private def iload(index: Byte): Unit = throw new NotImplementedError("iload not implemented")

  private def iload_n(n: Int): Unit =
    operandStack.push(localVariables(n))

  private def lload(index: Byte): Unit = throw new NotImplementedError("lload not implemented")

  private def lload_n(n: Int): Unit = throw new NotImplementedError("lload_n not implemented")

  private def fload(index: Byte): Unit =
    operandStack.push(localVariables.getFloat(index))

  private def fload_n(n: Byte): Unit =
    fload(n)

  private def dload(index: Byte): Unit = throw new NotImplementedError("dload not implemented")

  private def dload_n(n: Int): Unit = throw new NotImplementedError("dload_n not implemented")

  private def aload(index: UByte): Unit = {
    localVariables(index.toInt) match {
      case r: FValue.FValueReference => operandStack.push(r)
      case wut => throw new RuntimeException(s"aload was expecting ref, got $wut")
    }
  }

  private def aload_n(n: Int): Unit =
    aload(UByte(n))

  // ===== Array loads =====

  private def iaload(): Unit = throw new NotImplementedError("iaload not implemented")

  private def laload(): Unit = throw new NotImplementedError("laload not implemented")

  private def faload(): Unit = throw new NotImplementedError("faload not implemented")

  private def daload(): Unit = throw new NotImplementedError("daload not implemented")

  private def aaload(): Unit = throw new NotImplementedError("aaload not implemented")

  private def baload(): Unit = throw new NotImplementedError("baload not implemented")

  private def caload(): Unit = throw new NotImplementedError("caload not implemented")

  private def saload(): Unit = throw new NotImplementedError("saload not implemented")

  // ===== Stores =====

  private def istore(index: Byte): Unit = throw new NotImplementedError("istore not implemented")

  private def istore_n(n: Int): Unit = {
    val i = operandStack.pop() match {
      case i: FValueInt => i
      case _ => !!!
    }
    localVariables(n) = i
  }

  private def lstore(index: Byte): Unit = throw new NotImplementedError("lstore not implemented")

  private def lstore_n(n: Int): Unit = throw new NotImplementedError("lstore_n not implemented")

  private def fstore(index: Byte): Unit = throw new NotImplementedError("fstore not implemented")

  private def fstore_n(n: Int): Unit = throw new NotImplementedError("fstore_n not implemented")

  private def dstore(index: Byte): Unit = throw new NotImplementedError("dstore not implemented")

  private def dstore_n(n: Int): Unit = throw new NotImplementedError("dstore_n not implemented")

  private def astore(index: Byte): Unit = throw new NotImplementedError("astore not implemented")

  private def astore_n(n: Int): Unit = throw new NotImplementedError("astore_n not implemented")

  // ===== Array stores =====

  private def iastore(): Unit = throw new NotImplementedError("iastore not implemented")

  private def lastore(): Unit = throw new NotImplementedError("lastore not implemented")

  private def fastore(): Unit = throw new NotImplementedError("fastore not implemented")

  private def dastore(): Unit = throw new NotImplementedError("dastore not implemented")

  private def aastore(): Unit = throw new NotImplementedError("aastore not implemented")

  private def bastore(): Unit = throw new NotImplementedError("bastore not implemented")

  private def castore(): Unit = {
    val value = operandStack.popInt()
    val index = operandStack.popInt()
    val arrayRef = operandStack.popRef()
    heap.writeBytes(arrayRef.toHeapAddr, index.value, value.toBytes)
  }

  private def sastore(): Unit = throw new NotImplementedError("sastore not implemented")

  // ===== Stack management =====

  private def pop(): Unit = throw new NotImplementedError("pop not implemented")

  private def pop2(): Unit = throw new NotImplementedError("pop2 not implemented")

  private def dup(): Unit =
    operandStack.push(operandStack.head)

  private def dup_x1(): Unit = throw new NotImplementedError("dup_x1 not implemented")

  private def dup_x2(): Unit = throw new NotImplementedError("dup_x2 not implemented")

  private def dup2(): Unit = throw new NotImplementedError("dup2 not implemented")

  private def dup2_x1(): Unit = throw new NotImplementedError("dup2_x1 not implemented")

  private def dup2_x2(): Unit = throw new NotImplementedError("dup2_x2 not implemented")

  private def swap(): Unit = throw new NotImplementedError("swap not implemented")

  // ===== Arithmetic =====

  private def iadd(): Unit =
    operandStack.push(operandStack.popInt() + operandStack.popInt())

  private def ladd(): Unit = throw new NotImplementedError("ladd not implemented")

  private def fadd(): Unit = throw new NotImplementedError("fadd not implemented")

  private def dadd(): Unit = throw new NotImplementedError("dadd not implemented")

  private def isub(): Unit = throw new NotImplementedError("isub not implemented")

  private def lsub(): Unit = throw new NotImplementedError("lsub not implemented")

  private def fsub(): Unit = throw new NotImplementedError("fsub not implemented")

  private def dsub(): Unit = throw new NotImplementedError("dsub not implemented")

  private def imul(): Unit = throw new NotImplementedError("imul not implemented")

  private def lmul(): Unit = throw new NotImplementedError("lmul not implemented")

  private def fmul(): Unit = throw new NotImplementedError("fmul not implemented")

  private def dmul(): Unit = throw new NotImplementedError("dmul not implemented")

  private def idiv(): Unit = throw new NotImplementedError("idiv not implemented")

  private def ldiv(): Unit = throw new NotImplementedError("ldiv not implemented")

  private def fdiv(): Unit = throw new NotImplementedError("fdiv not implemented")

  private def ddiv(): Unit = throw new NotImplementedError("ddiv not implemented")

  private def irem(): Unit = throw new NotImplementedError("irem not implemented")

  private def lrem(): Unit = throw new NotImplementedError("lrem not implemented")

  private def frem(): Unit = throw new NotImplementedError("frem not implemented")

  private def drem(): Unit = throw new NotImplementedError("drem not implemented")

  private def ineg(): Unit = throw new NotImplementedError("ineg not implemented")

  private def lneg(): Unit = throw new NotImplementedError("lneg not implemented")

  private def fneg(): Unit = throw new NotImplementedError("fneg not implemented")

  private def dneg(): Unit = throw new NotImplementedError("dneg not implemented")

  private def ishl(): Unit = throw new NotImplementedError("ishl not implemented")

  private def lshl(): Unit = throw new NotImplementedError("lshl not implemented")

  private def ishr(): Unit = throw new NotImplementedError("ishr not implemented")

  private def lshr(): Unit = throw new NotImplementedError("lshr not implemented")

  private def iushr(): Unit = throw new NotImplementedError("iushr not implemented")

  private def lushr(): Unit = throw new NotImplementedError("lushr not implemented")

  private def iand(): Unit = throw new NotImplementedError("iand not implemented")

  private def land(): Unit = throw new NotImplementedError("land not implemented")

  private def ior(): Unit = throw new NotImplementedError("ior not implemented")

  private def lor(): Unit = throw new NotImplementedError("lor not implemented")

  private def ixor(): Unit = throw new NotImplementedError("ixor not implemented")

  private def lxor(): Unit = throw new NotImplementedError("lxor not implemented")

  private def iinc(index: Byte, const: Byte): Unit = throw new NotImplementedError("iinc not implemented")

  // ===== Conversions =====

  private def i2l(): Unit = throw new NotImplementedError("i2l not implemented")

  private def i2f(): Unit = throw new NotImplementedError("i2f not implemented")

  private def i2d(): Unit = throw new NotImplementedError("i2d not implemented")

  private def l2i(): Unit = throw new NotImplementedError("l2i not implemented")

  private def l2f(): Unit = throw new NotImplementedError("l2f not implemented")

  private def l2d(): Unit = throw new NotImplementedError("l2d not implemented")

  private def f2i(): Unit = throw new NotImplementedError("f2i not implemented")

  private def f2l(): Unit = throw new NotImplementedError("f2l not implemented")

  private def f2d(): Unit = throw new NotImplementedError("f2d not implemented")

  private def d2i(): Unit = throw new NotImplementedError("d2i not implemented")

  private def d2l(): Unit = throw new NotImplementedError("d2l not implemented")

  private def d2f(): Unit = throw new NotImplementedError("d2f not implemented")

  private def i2b(): Unit = throw new NotImplementedError("i2b not implemented")

  private def i2c(): Unit = throw new NotImplementedError("i2c not implemented")

  private def i2s(): Unit = throw new NotImplementedError("i2s not implemented")

  // ===== Comparisons =====

  private def lcmp(): Unit = throw new NotImplementedError("lcmp not implemented")

  private def fcmpl(): Unit = {
    val v2 = operandStack.popFloat().value
    val v1 = operandStack.popFloat().value
    if (v1.isNaN || v2.isNaN) {
      operandStack.push(FValue.FValueInt(-1))
    } else {
      if v1 > v2 then operandStack.push(FValue.FValueInt(1))
      else if v1 == v2 then operandStack.push(FValue.FValueInt(0))
      else if v1 < v2 then operandStack.push(FValue.FValueInt(-1))
    }
  }

  private def fcmpg(): Unit = {
    val v2 = operandStack.popFloat().value
    val v1 = operandStack.popFloat().value
    if (v1.isNaN || v2.isNaN) {
      operandStack.push(FValue.FValueInt(1))
    } else {
      if v1 > v2 then operandStack.push(FValue.FValueInt(1))
      else if v1 == v2 then operandStack.push(FValue.FValueInt(0))
      else if v1 < v2 then operandStack.push(FValue.FValueInt(-1))
    }
  }

  private def dcmpl(): Unit = throw new NotImplementedError("dcmpl not implemented")

  private def dcmpg(): Unit = throw new NotImplementedError("dcmpg not implemented")

  // ===== Control transfer =====

  private def ifcond(cond: Byte, branchbyte1: Byte, branchbyte2: Byte): Unit = {
    // branchbyte1/branchbyte2 were consumed via readNext before this call, so frame.pc
    // currently sits 2 bytes past the if<cond> opcode itself.
    val opcodePc = frame.pc - 2
    // The offset is a *signed* 16-bit value (loops need negative/backward offsets).
    val offset: Short = (((branchbyte1 & 0xff) << 8) | (branchbyte2 & 0xff)).toShort
    val value = operandStack.popInt()
    val taken = cond match {
      case 0 => value.value == 0
      case 1 => value.value != 0
      case 2 => value.value < 0
      case 3 => value.value >= 0
      case 4 => value.value > 0
      case 5 => value.value <= 0
      case other => throw new Exception(s"Unkown cond $other")
    }
    // The offset is relative to the if<cond> instruction's own address. runLoop() adds 1
    // to frame.pc after every dispatched opcode, so land 1 byte short to compensate.
    if (taken) frame.pc = opcodePc + offset - 1
  }

  private def if_icmp_cond(cond: Byte, branchbyte1: Byte, branchbyte2: Byte): Unit =
    throw new NotImplementedError("if_icmp_cond not implemented")

  private def if_acmp_cond(cond: Byte, branchbyte1: Byte, branchbyte2: Byte): Unit =
    throw new NotImplementedError("if_acmp_cond not implemented")

  private def goto(branchbyte1: Byte, branchbyte2: Byte): Unit = throw new NotImplementedError("goto not implemented")

  private def jsr(branchbyte1: Byte, branchbyte2: Byte): Unit = throw new NotImplementedError("jsr not implemented")

  private def ret(index: Byte): Unit = throw new NotImplementedError("ret not implemented")

  // ===== Returns =====

  private def typedReturn(kind: Byte): Unit = throw new NotImplementedError("typedReturn not implemented")

  // ===== Field access =====

  private def getstatic(index1: Byte, index2: Byte) = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val field = constantPool.resolveFieldref(idx)
    val clazz = field.clazz.resolveClazz(classLoader)
    val value = clazz.staticFields((field.nameAndType.name.value, field.nameAndType.descriptor.value))
    operandStack.push(value)
  }

  private def putstatic(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val field = constantPool.resolveFieldref(idx)
    val clazz = field.clazz.resolveClazz(classLoader)
    clazz.staticFields(field.nameAndType.toTupleSt) = operandStack.pop()
  }

  private def getfield(index1: Byte, index2: Byte): Unit = throw new NotImplementedError("getfield not implemented")

  private def putfield(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val field = constantPool.resolveFieldref(idx)
    val clazz = field.clazz.resolveClazz(classLoader)
    val fieldRes = clazz.instanceFields(field.toTuple)
    val value = operandStack.pop()
    val ref = operandStack.popRef()
    heap.setField(clazz, fieldRes, ref.toHeapAddr, value)
  }

  // ===== Method invocation =====

  private def invokevirtual(index1: Byte, index2: Byte): Unit =
    throw new NotImplementedError("invokevirtual not implemented")

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
    clazz.resolveSpecialMethod(fname, fdesc, objectClazz) match {
      case j: JvmMethod => invokeMethod(j, clazz).run()
      case com.romic.fun_jvm.NativeMethod(_, _, _) => ???
    }

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

  private def invokeinterface(index1: Byte, index2: Byte, count: Byte, zero: Byte): Unit =
    throw new NotImplementedError("invokeinterface not implemented")

  private def invokedynamic(index1: Byte, index2: Byte, zero1: Byte, zero2: Byte): Unit =
    throw new NotImplementedError("invokedynamic not implemented")

  // ===== Object/array creation and type checks =====

  private def new_(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val clazz = constantPool.resolveClass(idx).resolveClazz(classLoader)
    operandStack.push(heap.storeNew(clazz).toRef(clazz.name))
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
    val ref = heap.allocateArray(arraytype, operandStack.popInt().value)
    operandStack.push(ref.toRef(FType.FTypeArray(arraytype).toString))
  }

  private def anewarray(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val classRef = constantPool.resolveClass(idx).resolveClazz(classLoader)
    val type_ = FType.FTypeReference(classRef.name)
    val ref = heap.allocateArray(type_, operandStack.popInt().value).toRef
    operandStack.push(ref(classRef.name))
  }

  private def arraylength(): Unit = throw new NotImplementedError("arraylength not implemented")

  private def athrow(): Unit = throw new NotImplementedError("athrow not implemented")

  private def checkcast(index1: Byte, index2: Byte): Unit = throw new NotImplementedError("checkcast not implemented")

  private def instanceof(index1: Byte, index2: Byte): Unit = throw new NotImplementedError("instanceof not implemented")

  // ===== Synchronization =====

  private def monitorenter(): Unit = throw new NotImplementedError("monitorenter not implemented")

  private def monitorexit(): Unit = throw new NotImplementedError("monitorexit not implemented")

  // ===== Extended (wide/tableswitch/lookupswitch skipped: variable-length operand encoding) =====

  private def multianewarray(index1: Byte, index2: Byte, dimensions: Byte): Unit =
    throw new NotImplementedError("multianewarray not implemented")

  private def ifnull_cond(cond: Byte, branchbyte1: Byte, branchbyte2: Byte): Unit =
    throw new NotImplementedError("ifnull_cond not implemented")

  private def goto_w(b1: Byte, b2: Byte, b3: Byte, b4: Byte): Unit =
    throw new NotImplementedError("goto_w not implemented")

  private def jsr_w(b1: Byte, b2: Byte, b3: Byte, b4: Byte): Unit =
    throw new NotImplementedError("jsr_w not implemented")

  // ===== Reserved (never emitted by any legitimate compiler) =====

  private def breakpoint(): Unit = throw new NotImplementedError("breakpoint not implemented")

  private def impdep1(): Unit = throw new NotImplementedError("impdep1 not implemented")

  private def impdep2(): Unit = throw new NotImplementedError("impdep2 not implemented")

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
