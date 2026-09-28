package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.FValue.FValueClassRef
import com.romic.fun_jvm.{
  BytecodeExecutor,
  ClassReferenceVal,
  InstanceClazz,
  FThreadState,
  FValue,
  MethodExecutorFactory
}

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
    "user.dir" -> ".",
    "file.encoding" -> "UTF-8",
    "sun.jnu.encoding" -> "UTF-8",
    "sun.stdout.encoding" -> "UTF-8",
    "sun.stderr.encoding" -> "UTF-8"
  )

  def initProperties(clazz: InstanceClazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = {
    val propRef = NativeArgs.param1[FValueClassRef](params, threadState)
    val (propsClazz, setPropertyMeth) = WKProperties.fun_setProperty(threadState.classLoader)
    defaultProperties.foreach { case (key, value) =>
      val keySt = threadState.heap.storeStringLiteral(
        key,
        threadState.classLoader,
        threadState.objectClazz
      ).toRef(WKString.className)
      val valueSt = threadState.heap.storeStringLiteral(
        value,
        threadState.classLoader,
        threadState.objectClazz
      ).toRef(WKString.className)
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

  def arraycopy(clazz: InstanceClazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = {
    val (src, srcPos, dest, destPos, length) = params match {
      case (src: FValue.FValueArrayRef) :: FValue.FValueInt(sp) :: (dest: FValue.FValueArrayRef) ::
          FValue.FValueInt(dp) :: FValue.FValueInt(len) :: _ =>
        (src, sp, dest, dp, len)
      case other =>
        throw new IllegalArgumentException(s"arraycopy expected (Object[],int,Object[],int,int), got $other")
    }
    threadState.heap.copyArrayRange(src.toHeapAddr, srcPos, dest.toHeapAddr, destPos, length)
    None
  }

  def setIn0(clazz: InstanceClazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = {
    val in = NativeArgs.param1[FValueClassRef](params, threadState)
    clazz.staticFields(("in", "Ljava/io/InputStream;")) = in
    None
  }

  def setOut0(clazz: InstanceClazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = {
    val out = NativeArgs.param1[FValueClassRef](params, threadState)
    clazz.staticFields(("out", "Ljava/io/PrintStream;")) = out
    None
  }

  def setErr0(clazz: InstanceClazz)(
    threadState: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = {
    val err = NativeArgs.param1[FValueClassRef](params, threadState)
    clazz.staticFields(("err", "Ljava/io/PrintStream;")) = err
    None
  }

}
