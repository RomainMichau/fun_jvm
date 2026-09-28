package com.romic.fun_jvm.well_known

import com.romic.fun_jvm.{FThreadState, FType, FValue, Heap, InstanceClazz, JavaInstanceField}

object WKField extends WellKnownClass {
  val className: String = "java/lang/reflect/Field"

  private def typeMirror(t: FType, s: FThreadState): Heap.Address = t match {
    case FType.FTypeClassRef(name) => s.classLoader.getWithoutInit(name).getClassMirror(s.classLoader)
    case arr: FType.FTypeArray => s.classLoader.getWithoutInit(arr.toString).getClassMirror(s.classLoader)
    case FType.FTypeBoolean => s.classLoader.primitiveClass("boolean")
    case FType.FTypeByte => s.classLoader.primitiveClass("byte")
    case FType.FTypeChar => s.classLoader.primitiveClass("char")
    case FType.FTypeShort => s.classLoader.primitiveClass("short")
    case FType.FTypeInt => s.classLoader.primitiveClass("int")
    case FType.FTypeLong => s.classLoader.primitiveClass("long")
    case FType.FTypeFloat => s.classLoader.primitiveClass("float")
    case FType.FTypeDouble => s.classLoader.primitiveClass("double")
    case FType.Void => throw new RuntimeException("void field type")
  }

  def newField(
    s: FThreadState,
    declaringClazz: InstanceClazz,
    name: String,
    type_ : FType,
    accessFlags: Int
  ): FValue.FValueClassRef = {
    val fieldClazz = getClazz(s.classLoader)
    val addr = s.heap.new_(fieldClazz)

    def set(fieldName: String, fieldType: FType, value: FValue): Unit = {
      val (owner, f) = fieldClazz.resolveField(fieldName, fieldType)
      s.heap.setField(owner, f, addr, value, s.classLoader, s.objectClazz)
    }

    set(
      "clazz",
      FType.FTypeClassRef.of(WKClass.className),
      declaringClazz.getClassMirror(s.classLoader).toRef(WKClass.className)
    )
    set(
      "name",
      FType.FTypeClassRef.of(WKString.className),
      s.heap.storeStringLiteral(name, s.classLoader, s.objectClazz).toRef(WKString.className)
    )
    set("type", FType.FTypeClassRef.of(WKClass.className), typeMirror(type_, s).toRef(WKClass.className))
    set("modifiers", FType.FTypeInt, FValue.FValueInt(accessFlags))
    set("slot", FType.FTypeInt, FValue.FValueInt(0))
    addr.toRef(fieldClazz.name)
  }

  def resolveDeclaredField(s: FThreadState, fieldRef: FValue.FValueClassRef): (InstanceClazz, JavaInstanceField) = {
    val fieldClazz = getClazz(s.classLoader)

    def get(fieldName: String, fieldType: FType): FValue = {
      val (owner, f) = fieldClazz.resolveField(fieldName, fieldType)
      s.heap.getField(owner, f, fieldRef.toHeapAddr, s.classLoader)
    }

    val declaringClazzRef = get("clazz", FType.FTypeClassRef.of(WKClass.className)) match {
      case r: FValue.FValueClassRef => r
      case other => throw new RuntimeException(s"expected clazz field to be a Class ref, got $other")
    }
    val declaringClazz = WKClass.getUnderlyingClazz(declaringClazzRef, s) match {
      case c: InstanceClazz => c
      case other => throw new RuntimeException(s"expected an instance class, got $other")
    }
    val name = get("name", FType.FTypeClassRef.of(WKString.className)) match {
      case r: FValue.FValueClassRef => s.heap.readString(r, s.classLoader)
      case other => throw new RuntimeException(s"expected name field to be a String ref, got $other")
    }
    declaringClazz.instancesField.values.find(_.name == name) match {
      case Some(f) => (declaringClazz, f)
      case None => throw new NoSuchFieldError(s"$name not found on ${declaringClazz.name}")
    }
  }

}
