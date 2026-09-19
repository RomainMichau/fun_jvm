import ClassFileInfo.ConstantPool.LongOrDouble.Double_
import ClassFileInfo.ConstantPool.LongOrDouble.Long_
import ClassFileInfo.{Attribute, ConstantPool, UInt, UShort}
import FValue.FInt
import Interpreter.!!!
import Utils.to

import scala.collection.mutable

type LocalVariables = Array[FValue]
type operand = Byte

object Frame {
  def apply(maxLocals: Int): Frame = Frame(Array.fill(maxLocals)(null), mutable.Stack.empty)
}

case class Frame(localVariables: LocalVariables = Array.empty, operandStack: mutable.Stack[FValue] = mutable.Stack.empty)

case class FThread(var pc: Int = 0, stack: mutable.Stack[Frame] = mutable.Stack(Frame()))

object Interpreter {
  def apply(code: Attribute.Code, constantPool: ConstantPool): Interpreter = {
    val initFrame = Frame(code.maxLocals.toInt)
    val thead = FThread(0, mutable.Stack(initFrame))
    new Interpreter(thead, code.code, constantPool)
  }

  def !!! : Nothing = throw new RuntimeException("UnexpectedType")
}

class Interpreter(thread: FThread, code: Vector[Byte], constantPool: ConstantPool) {
  private val frame = thread.stack.head
  private val operandStack = thread.stack.head.operandStack
  private val localVariables = thread.stack.head.localVariables

  private def readNext: Byte = {
    thread.pc += 1
    code(thread.pc)
  }

  def run(): Unit = {
    def runLoop(): Unit = {
      if (thread.pc < code.size) {
        code(thread.pc) match {
          case x if (OpCode.iconst_m1 to OpCode.iconst_5).contains(x) => iconst_i(x - OpCode.iconst_0)
          case x if (OpCode.istore_0 to OpCode.istore_3).contains(x) => istore_n(x - OpCode.istore_0)
          case OpCode.return_ => return
          case ldc2_w_ => ldc2_w(readNext, readNext)
          case uh => throw new RuntimeException(s"watdat opcode ? 0x${(uh & 0xff).toHexString} / ${uh}")
        }
        thread.pc += 1
        runLoop()
      }
    }

    runLoop()
  }


  private def ldc2_w(index1: Byte, index2: Byte) = {
    val idx = UShort(((index1 & 0xff) << 8) | (index2 & 0xff))
    val toPush = constantPool.resolveLongOrDouble(idx).toOption.get match {
      case Double_(d) => FValue.FDouble(d)
      case Long_(l) => FValue.FLong(l)
    }
    operandStack.push(toPush)
  }

  private def iconst_i(i: Int): Unit = {
    operandStack.push(FInt(i))
  }

  private def istore_n(n: Int): Unit = {
    val i = operandStack.pop() match {
      case i: FInt => i
      case _ => !!!
    }
    localVariables(n) = i
  }
}
