package com.romic.fun_jvm.utils

import com.romic.fun_jvm.*
import com.romic.fun_jvm.FValue.{FValueArrayRef, FValueClassRef}
import com.romic.fun_jvm.classloader.FClassLoader

import scala.annotation.tailrec

object Assignability {

  @tailrec
  def isAssignable(sRef: FValueRef, tRef: FTypeRef, classLoader: FClassLoader, objectClazz: InstanceClazz): Boolean = {
    val tClazz = classLoader.getClass(tRef.className)
    sRef match {
      case FValueClassRef(Some(value)) =>
        val sClazz = classLoader.getInstanceClass(value.className)
        if (!sClazz.isInterface) {
          tClazz match {
            case tClazz: InstanceClazz if tClazz.isNotInterface => (sClazz == tClazz) || sClazz.isASubClassOf(tClazz)
            case tClazz: InstanceClazz if tClazz.isInterface => sClazz.implementInterfaceOf(tClazz)
            case _ => false
          }
        } else {
          tClazz match {
            case tClazz: InstanceClazz if tClazz.isNotInterface => tClazz == objectClazz
            case tClazz: InstanceClazz if tClazz.isInterface => tClazz == sClazz || tClazz.isASuperClassOf(sClazz)
            case _ => false
          }
        }
      case FValueArrayRef(Some(sArrRef)) =>
        val sClazz = classLoader.getArrayClass(sArrRef.className)
        (tClazz, sClazz) match {
          case (tClazz: InstanceClazz, _) if tClazz.isNotInterface => tClazz == objectClazz
          case (tClazz: InstanceClazz, _) if tClazz.isInterface =>
            tClazz.name == "java/lang/Cloneable" || tClazz.name == "java/io/Serializable"
          case (tClazz: ArrayClazz, sClazz: ArrayClazz) =>
            tClazz.innerType == sClazz.innerType ||
            ((tClazz.innerType, sClazz.innerType) match {
              case (tRef: FValueRef, sRef: FValueRef) => isAssignable(sRef, tRef.getType, classLoader, objectClazz)
              case _ => false
            })
          case _ => false
        }
      case FValueArrayRef(None) =>
        throw new IllegalStateException("isAssignable expects a non-null reference; callers must check isNull first")
      case FValueClassRef(None) =>
        throw new IllegalStateException("isAssignable expects a non-null reference; callers must check isNull first")
    }
  }

}
