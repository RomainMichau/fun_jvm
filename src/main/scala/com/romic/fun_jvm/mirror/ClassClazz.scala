package com.romic.fun_jvm.mirror

import com.romic.fun_jvm.{Clazz, Heap, RuntimeConstantPool}

import scala.collection.mutable

// Clazz of java.lang.Clazz
object ClassClazz {
  private var classClazz: Option[Clazz] = None

  def getClassClazz(heap: Heap) = classClazz match {
    case Some(c) => c
    case None =>
      val clazz = new Clazz(
        "java.lang.Clazz",
        false,
        mutable.Map.empty,
        RuntimeConstantPool.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        List(("name", "Ljava/lang/String;")),
        None,
        List.empty,
        heap
      )
      classClazz = Some(clazz)
      clazz
  }

}
