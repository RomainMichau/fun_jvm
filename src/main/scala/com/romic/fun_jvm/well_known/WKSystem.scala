package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.FValue.FValueClassRef
import com.romic.fun_jvm.{BytecodeExecutor, ClassReferenceVal, Clazz, FThreadState, FValue, MethodExecutorFactory}

object WKSystem extends WellKnownClass {
  val className: String = "java/lang/System"

  private val defaultProperties: Map[String, String] = Map(
    "java.version" -> "1.0",
    "java.vendor" -> "fun_jvm",
    "java.vendor.url" -> "https://example.com",
    "java.home" -> "/opt/fun_jvm",
    "java.class.version" -> "52.0",
    "java.class.path" -> ".",
    "os.name" -> "Linux",
    "os.arch" -> "amd64",
    "os.version" -> "1.0",
    "file.separator" -> "/",
    "path.separator" -> ":",
    "line.separator" -> "\n",
    "user.name" -> "user",
    "user.home" -> "/home/user",
    "user.dir" -> "."
  )

  def initProperties(clazz: Clazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = {
    val propRef = params.head match {
      case t: FValueClassRef => t
      case wut => throw new IllegalArgumentException(s"initProperties expect a Propertie in input, got $wut")
    }
    val (propsClazz, setPropertyMeth) = WKProperties.fun_setProperty(threadState.classLoader)
    defaultProperties.foreach { case (key, value) =>
      val keySt = threadState.heap.storeStringLiteral(key, threadState.classLoader).toRef(WKString.className)
      val valueSt = threadState.heap.storeStringLiteral(value, threadState.classLoader).toRef(WKString.className)
      val params = List(keySt, valueSt)
      executorFactory.generateExecutorForMethod(
        setPropertyMeth,
        propsClazz,
        threadState,
        BytecodeExecutor.sinkReturn,
        params,
        Some(propRef)
      ).run()
    }
    Some(propRef)
  }

}
