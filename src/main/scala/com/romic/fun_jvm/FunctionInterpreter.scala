package com.romic.fun_jvm

import FValue.{FValueByte, FValueInt}
import FunctionInterpreter.!!!
import com.romic.fun_jvm.utils.Utils.{UInt, UShort, to, toInt0Ext}
import RuntimeConstantPool.LongOrDouble.Double_
import RuntimeConstantPool.LongOrDouble.Long_
import scala.collection.mutable

type LocalVariables = Array[FValue]
type operand = Byte

object Frame {
  def apply(maxLocals: Int): Frame = Frame(Array.fill(maxLocals)(null), mutable.Stack.empty)
}

case class Frame(localVariables: LocalVariables = Array.empty, operandStack: mutable.Stack[FValue] = mutable.Stack.empty) {
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

case class FThread(var pc: Int = 0, stack: mutable.Stack[Frame] = mutable.Stack(Frame()))

object FunctionInterpreter {
  def apply(method: JvmMethod, clazz: Clazz, classLoader: ClassLoader): FunctionInterpreter = {
    val initFrame = Frame(method.maxLocals.toInt)
    val thead = FThread(0, mutable.Stack(initFrame))
    new FunctionInterpreter(thead, method, clazz, classLoader)
  }

  def !!! : Nothing = throw new RuntimeException("UnexpectedType")
}

class FunctionInterpreter(thread: FThread, method: JvmMethod, clazz: Clazz, classLoader: ClassLoader) {
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

  def run(): Unit = {
//    println(constantPool.toVector.zipWithIndex.map(x => (x._2, x._1)).mkString("\n"))
    println(s"================== NOW RUNNING ${clazz.name}.${method.methodName} ${method.methodDescriptor}")
    println(code.map(_.toInt0Ext.toHexString).mkString(" "))

    def runLoop(): Unit = {
      if (thread.pc < code.size) {
        println(code(thread.pc).toInt0Ext.toHexString)
        code(thread.pc) match {
          case x if (OpCode.iconst_m1 to OpCode.iconst_5).contains(x) => iconst_i(x - OpCode.iconst_0)
          case x if (OpCode.istore_0 to OpCode.istore_3).contains(x) => istore_n(x - OpCode.istore_0)
          case x if (OpCode.iload_0 to OpCode.iload_3).contains(x) => iload_n(x - OpCode.iload_0)
          case OpCode.bipush => bipush(readNext)
          case OpCode.putstatic => putstatic(readNext, readNext)
          case OpCode.return_ => return
          case OpCode.getstatic => getstatic(readNext, readNext)
          case OpCode.invokestatic => invokestatic(readNext, readNext)
          case OpCode.iadd => iadd()
          case OpCode.ldc2_w => ldc2_w(readNext, readNext)
          case uh =>
            debug()
            throw new RuntimeException(s"watdat opcode ? 0x${(uh & 0xff).toHexString} / ${uh}")
        }
        thread.pc += 1
        runLoop()
      }
    }

    runLoop()
    println(frame.debug)
  }

  private def invokestatic(index1: Byte, index2: Byte): Unit = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val methodRef = constantPool.resolveMethodRef(idx)
    val (clazz, method) = methodRef.resolveMethod(classLoader)
    println(s"inkovestatic ${clazz.name} ${method.methodName}")
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

  private def ldc2_w(index1: Byte, index2: Byte) = {
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
    val currentOp = if (thread.pc < code.size) f"0x${code(thread.pc) & 0xff}%02x" else "<end of code>"
    println(
      s"""================== DEBUG ${clazz.name}.${method.methodName}${method.methodDescriptor}
         |pc: ${thread.pc}
         |current opcode: $currentOp
         |call stack depth: ${thread.stack.size}
         |${thread.stack.zipWithIndex.map { case (f, i) => s"--- frame $i${if (i == 0) " (current)" else ""} ---\n${f.debug}" }.mkString("\n")}
         |""".stripMargin
    )
  }
}