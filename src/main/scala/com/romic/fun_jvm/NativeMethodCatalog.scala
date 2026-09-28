package com.romic.fun_jvm

import com.romic.fun_jvm.FValue.FValueClassRef
import com.romic.fun_jvm.well_known.{
  WKAccessController,
  WKClass,
  WKDouble,
  WKFloat,
  WKObject,
  WKReflection,
  WKString,
  WKSystem,
  WKThread,
  WKThrowable,
  WKUnsafe
}

type NativeMethodRunWithClazz =
  (clazz: InstanceClazz) => (
    FThreadState,
    MethodExecutorFactory,
    List[FValue],
    Option[FValueClassRef]
  ) => Option[FValue]

type NativeMethodRun = (FThreadState, MethodExecutorFactory, List[FValue], Option[FValueClassRef]) => Option[FValue]

class NativeMethodCatalog(heap: Heap) {

  // Applies to registerNatives regardless of which class declares it, so it can't live in a
  // single WK<Class> file the way the other native methods below do.
  private def noOp(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValueClassRef]
  ): Option[FValue] = None

  def get(clazz: InstanceClazz, method: NativeMethod): NativeMethodRun = {
    println(s"Preparing method ${clazz.name} $method")
    val run: NativeMethodRunWithClazz = (clazz.name, method.methodName, method.methodDescriptor.toString) match {
      case (_, "registerNatives", _) => noOp
      case ("java/lang/Class", "desiredAssertionStatus0", "(Ljava/lang/Class;)Z") => WKClass.desiredAssertionStatus0
      // TODO: stub for now, should allocate/cache a Class mirror per primitive name via
      // heap.storeNew(ClassClazz.getClassClazz(heap)) + setField("name", ...) and return it
      case ("java/lang/Class", "getPrimitiveClass", "(Ljava/lang/String;)Ljava/lang/Class;") =>
        WKClass.getPrimitiveClass
      case ("java/lang/Class", "getName0", "()Ljava/lang/String;") => WKClass.getName0
      case ("java/lang/Class", "getDeclaredFields0", "(Z)[Ljava/lang/reflect/Field;") => WKClass.getDeclaredFields0
      case ("java/lang/Class", "isPrimitive", "()Z") => WKClass.isPrimitive
      case ("java/lang/Class", "isAssignableFrom", "(Ljava/lang/Class;)Z") => WKClass.isAssignableFrom
      case (
            "java/lang/Class",
            "forName0",
            "(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;"
          ) =>
        WKClass.forName0
      case ("java/lang/System", "initProperties", "(Ljava/util/Properties;)Ljava/util/Properties;") =>
        WKSystem.initProperties
      case ("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V") => WKSystem.arraycopy
      case ("java/lang/System", "setIn0", "(Ljava/io/InputStream;)V") => WKSystem.setIn0
      case ("java/lang/System", "setOut0", "(Ljava/io/PrintStream;)V") => WKSystem.setOut0
      case ("java/lang/System", "setErr0", "(Ljava/io/PrintStream;)V") => WKSystem.setErr0
      case ("java/lang/Float", "floatToRawIntBits", "(F)I") => WKFloat.floatToRawIntBits
      case ("java/lang/Double", "doubleToLongBits", "(D)J") => WKDouble.doubleToLongBits
      case ("java/lang/Double", "doubleToRawLongBits", "(D)J") => WKDouble.doubleToRawLongBits
      case ("java/lang/Double", "longBitsToDouble", "(J)D") => WKDouble.longBitsToDouble
      case ("sun/misc/VM", "initialize", "()V") => noOp
      case ("java/io/FileInputStream", "initIDs", "()V") => noOp
      case ("java/io/FileDescriptor", "initIDs", "()V") => noOp
      case ("java/io/FileOutputStream", "initIDs", "()V") => noOp
      case (WKObject.className, "hashCode", "()I") => WKObject.hashCode_
      case (WKObject.className, "getClass", "()Ljava/lang/Class;") => WKObject.getClass_
      case (WKUnsafe.className, "arrayBaseOffset", "(Ljava/lang/Class;)I") => WKUnsafe.arrayBaseOffset
      case (WKUnsafe.className, "arrayIndexScale", "(Ljava/lang/Class;)I") => WKUnsafe.arrayIndexScale
      case (WKUnsafe.className, "addressSize", "()I") => WKUnsafe.addressSize
      case (
            WKUnsafe.className,
            "compareAndSwapObject",
            "(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z"
          ) =>
        WKUnsafe.compareAndSwapObject
      case (WKUnsafe.className, "objectFieldOffset", "(Ljava/lang/reflect/Field;)J") => WKUnsafe.objectFieldOffset
      case (WKUnsafe.className, "getIntVolatile", "(Ljava/lang/Object;J)I") => WKUnsafe.getIntVolatile
      case (WKUnsafe.className, "compareAndSwapInt", "(Ljava/lang/Object;JII)Z") => WKUnsafe.compareAndSwapInt
      case (WKReflection.className, "getCallerClass", "()Ljava/lang/Class;") => WKReflection.getCallerClass
      case (WKReflection.className, "getClassAccessFlags", "(Ljava/lang/Class;)I") =>
        WKReflection.getClassAccessFlags
      case (
            WKAccessController.className,
            "doPrivileged",
            "(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;"
          ) =>
        WKAccessController.doPrivileged
      case (WKAccessController.className, "doPrivileged", "(Ljava/security/PrivilegedAction;)Ljava/lang/Object;") =>
        WKAccessController.doPrivileged
      case (
            WKAccessController.className,
            "getStackAccessControlContext",
            "()Ljava/security/AccessControlContext;"
          ) =>
        WKAccessController.getStackAccessControlContext
      case (WKThread.className, "currentThread", "()Ljava/lang/Thread;") => WKThread.currentThread
      case (WKThread.className, "setPriority0", "(I)V") => noOp
      case (WKThread.className, "isAlive", "()Z") => WKThread.isAlive
      case (WKThread.className, "start0", "()V") => noOp
      case (WKThrowable.className, "fillInStackTrace", "(I)Ljava/lang/Throwable;") => WKThrowable.fillInStackTrace
      case (WKString.className, "intern", "()Ljava/lang/String;") => WKString.intern
      case other =>
        val static = if (method.isStatic) "static " else ""
        throw new NoSuchElementException(
          s"No native method registered for $static${clazz.name}.${method.methodName} ${method.methodDescriptor}"
        )
    }
    run(clazz)
  }

}
