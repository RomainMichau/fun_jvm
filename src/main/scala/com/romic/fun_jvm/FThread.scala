package com.romic.fun_jvm

import com.romic.fun_jvm.classloader.FClassLoader

import scala.collection.mutable

object Frame {

  def apply(maxLocals: Int, currentClass: Clazz, pc: Int = 0): Frame =
    new Frame(LocalVariables(maxLocals), OperandStack(), currentClass, pc)

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

object FrameStack {
  def apply(f: Frame): FrameStack = List(f)
}

opaque type FrameStack = List[Frame]

extension (s: FrameStack) {
  private def toList: List[Frame] = s

  def head: Frame = s.head
  def push(f: Frame): FrameStack = f +: s
  def size: Int = s.size
  def zipWithIndex: Seq[(Frame, Int)] = s.zipWithIndex

}

object FThread {

  def apply(pc: Int = 0, maxLocals: Int, currentClass: Clazz): FThread =
    new FThread(FrameStack(Frame(maxLocals, currentClass, pc)))

}

case class FThreadState(heap: Heap, classLoader: FClassLoader, thread: FThread) {
  val frame: Frame = thread.stack.head
  val operandStack: OperandStack = frame.operandStack
  val localVariables: LocalVariables = frame.localVariables
}

class FThread(val stack: FrameStack) {
  def push(frame: Frame): FThread = new FThread(stack.push(frame))
}
