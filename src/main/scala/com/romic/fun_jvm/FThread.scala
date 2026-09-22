package com.romic.fun_jvm

import scala.collection.mutable

object FThread {

  def apply(pc: Int = 0, maxLocals: Int, currentClass: Clazz): FThread =
    new FThread(mutable.Stack(Frame(maxLocals, currentClass, pc)))

}

case class FThreadState(heap: Heap, classLoader: FClassLoader, thread: FThread) {
  val frame: Frame = thread.stack.head
  val operandStack: OperandStack = frame.operandStack
  val localVariables: LocalVariables = frame.localVariables
}

class FThread(val stack: mutable.Stack[Frame])
