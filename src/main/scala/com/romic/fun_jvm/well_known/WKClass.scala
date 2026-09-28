package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.classloader.FClassLoader
import com.romic.fun_jvm.{
  ArrayClazz,
  BytecodeExecutor,
  ClassReferenceVal,
  Clazz,
  FThreadState,
  FType,
  FValue,
  FValueRef,
  InstanceClazz,
  MethodExecutorFactory,
  SecretInstanceField
}

object WKClass extends WellKnownClass {
  val className: String = "java/lang/Class"
  val mirrorKlazzIdField: String = "MirrorKlazzId"

  def mirrorKlazzIdSecretField(classLoader: FClassLoader): (InstanceClazz, SecretInstanceField) = {
    val clazz = getClazz(classLoader)
    val field = clazz.secretInstanceField((mirrorKlazzIdField, FType.FTypeInt))
    (clazz, field)
  }

  def getUnderlyingClazz(classRef: FValue.FValueClassRef, s: FThreadState): Clazz = classRef match {
    case FValue.FValueClassRef(Some(ClassReferenceVal(addr, this.className))) =>
      val (clazzCLass, field) = this.mirrorKlazzIdSecretField(s.classLoader)
      val classId = s.heap.getSecretFieldInt(field, addr)
      val clazz = s.classLoader.getClazzById(classId)
      clazz
    case FValue.FValueClassRef(None) => throw new NullPointerException
    case FValue.FValueClassRef(Some(_, wutClassName)) =>
      throw IllegalArgumentException(s"Expexting Class instance, got $wutClassName")
  }

  def desiredAssertionStatus0(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = Some(FValue.FValueBoolean(true))

  def getPrimitiveClass(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val name = NativeArgs.param1[String](params, s)
    Some(s.classLoader.primitiveClass(name).toRef(className))
  }

  def getName0(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val underlying = NativeArgs.this_[Clazz](this_, s)
    Some(s.heap.storeStringLiteral(underlying.name, s.classLoader, s.objectClazz).toRef(WKString.className))
  }

  def forName0(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val (name, init, _, _) = NativeArgs.param4[String, Boolean, Unit, Unit](params, s)
    val targetName = name.replace('.', '/')
    try {
      val targetClazz = if (init) s.classLoader.getClass(targetName) else s.classLoader.getWithoutInit(targetName)
      Some(targetClazz.getClassMirror(s.classLoader).toRef(WKClass.className))
    } catch {
      case FClassLoader.ClassProviderNotFound(_) =>
        val (ref, excClazz) = WKThrowable.newWithMessage("java/lang/ClassNotFoundException", name, s)
        throw BytecodeExecutor.NativeThrow(ref, excClazz)
    }
  }

  def isPrimitive(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val self = NativeArgs.this_[FValue.FValueClassRef](this_, s)
    Some(FValue.FValueBoolean(s.classLoader.isPrimitiveClassAddr(self.toHeapAddr)))
  }

  private def isAssignableClazz(from: Clazz, to: Clazz, classLoader: FClassLoader): Boolean = (from, to) match {
    case (f: InstanceClazz, t: InstanceClazz) =>
      f == t || (if (t.isInterface) f.implementInterfaceOf(t) else f.isASubClassOf(t))
    case (f: ArrayClazz, t: ArrayClazz) =>
      (f.innerType, t.innerType) match {
        case (fi: FType.FTypeClassRef, ti: FType.FTypeClassRef) =>
          isAssignableClazz(
            classLoader.getInstanceClass(fi.className),
            classLoader.getInstanceClass(ti.className),
            classLoader
          )
        case _ => f.innerType == t.innerType
      }
    case (_: ArrayClazz, t: InstanceClazz) =>
      t.name == "java/lang/Object" || t.name == "java/lang/Cloneable" || t.name == "java/io/Serializable"
    case _ => false
  }

  def isAssignableFrom(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val self = NativeArgs.this_[FValue.FValueClassRef](this_, s)
    val other = NativeArgs.param1[FValue.FValueClassRef](params, s)
    val result =
      if (self.toHeapAddr == other.toHeapAddr) true
      else if (
        s.classLoader.isPrimitiveClassAddr(self.toHeapAddr) || s.classLoader.isPrimitiveClassAddr(other.toHeapAddr)
      )
        false
      else isAssignableClazz(getUnderlyingClazz(other, s), getUnderlyingClazz(self, s), s.classLoader)
    Some(FValue.FValueBoolean(result))
  }

  def getDeclaredFields0(clazz: InstanceClazz)(
    s: FThreadState,
    executorFactory: MethodExecutorFactory,
    params: List[FValue],
    this_ : Option[FValue.FValueClassRef]
  ): Option[FValue] = {
    val declaringClazz = NativeArgs.this_[Clazz](this_, s) match {
      case c: InstanceClazz => c
      case other => throw new RuntimeException(s"getDeclaredFields0 expects an instance class, got $other")
    }

    val instanceFieldRefs =
      declaringClazz.instancesField.values.map(f =>
        WKField.newField(s, declaringClazz, f.name, f.type_, f.accessFlags)
      )
    val staticFieldRefs = declaringClazz.staticFields.keys.map { case (name, descriptor) =>
      WKField.newField(s, declaringClazz, name, FType.parse(descriptor), 0x8)
    }
    val allFieldRefs = (instanceFieldRefs ++ staticFieldRefs).toList

    val elementType = FType.FTypeClassRef.of(WKField.className)
    val arrAddr = s.heap.allocateArray(FType.FTypeArray.of(elementType), allFieldRefs.size)
    allFieldRefs.zipWithIndex.foreach { case (ref, i) => s.heap.writeElementInArr(arrAddr, i, ref) }
    Some(arrAddr.toArrRef(elementType))
  }

}
