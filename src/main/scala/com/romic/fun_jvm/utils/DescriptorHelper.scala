package com.romic.fun_jvm.utils

import com.romic.fun_jvm.Clazz.MethodDescriptor
import com.romic.fun_jvm.FType

import scala.annotation.tailrec

object DescriptorHelper {
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
