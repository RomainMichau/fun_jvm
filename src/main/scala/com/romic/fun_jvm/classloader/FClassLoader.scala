package com.romic.fun_jvm.classloader

import cats.data.Validated
import com.romic.fun_jvm.classparser.ClassFileInfo
import com.romic.fun_jvm.*
import com.romic.fun_jvm.classloader.FClassLoader.ClassId
import com.romic.fun_jvm.utils.Utils
import com.romic.fun_jvm.well_known.{WKClass, WKObject}

import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipFile}
import scala.collection.mutable
import scala.jdk.CollectionConverters
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.util.Using

trait ClassBytesProvider {
  def containsClass(className: String): Boolean

  def getClass(className: String): Array[Byte]

  def providerName: String
}

object JarClassProvider {

  def apply(jarPath: Path): JarClassProvider = {
    Using.resource(new ZipFile(jarPath.toFile)) { zip =>
      val classes = zip.entries().asScala.toList.map(x => x.getName -> x).toMap
      new JarClassProvider(jarPath, classes)
    }
  }

}

class JarClassProvider(jarPath: Path, classesEntries: Map[String, ZipEntry]) extends ClassBytesProvider {

  private def readClassBytes(className: String): Array[Byte] = {
    val entryName = className + ".class"
    Using.resource(new ZipFile(jarPath.toFile)) { zip =>
      val entry =
        classesEntries.getOrElse(entryName, throw new Exception(s"Unable to load class $className from $jarPath"))
      zip.entries().asScala.toList
      val in = zip.getInputStream(entry)
      try in.readAllBytes()
      finally in.close()
    }
  }

  def containsClass(className: String): Boolean = classesEntries.contains(className + ".class")

  def getClass(className: String): Array[Byte] = readClassBytes(className)

  def providerName: String = s"JarClassProvider $jarPath"
}

class ClassFileDirectoryProvider(classDir: Path) extends ClassBytesProvider {

  def containsClass(className: String): Boolean = classDir.resolve(className + ".class").toFile.exists()

  def getClass(className: String): Array[Byte] = Files.readAllBytes(classDir.resolve(className + ".class"))

  def providerName: String = s"ClassFileDirectoryProvider $classDir"
}

class ClassLoaderBuilder(providers: List[ClassBytesProvider] = List.empty) {
  def withJar(jarPath: Path): ClassLoaderBuilder = ClassLoaderBuilder(JarClassProvider(jarPath) +: providers)

  def withClassFileDir(classDir: Path): ClassLoaderBuilder =
    ClassLoaderBuilder(new ClassFileDirectoryProvider(classDir) +: providers)

  def build(heap: Heap, nativeMethodCatalog: NativeMethodCatalog): FClassLoader =
    new FClassLoader(providers, heap, nativeMethodCatalog)

}

object FClassLoader {
  type ClassId = Int

  case class ClassProviderNotFound(className: String) extends RuntimeException(s"No class provider contains $className")
}

class FClassLoader(providers: List[ClassBytesProvider], heap: Heap, nativeMethodCatalog: NativeMethodCatalog) {
  private val instancesClazzs: mutable.Map[String, InstanceClazz] = mutable.Map.empty
  private val arrayClazzs: mutable.Map[String, ArrayClazz] = mutable.Map.empty

  private val classByIds: mutable.ArrayBuffer[Clazz] = mutable.ArrayBuffer.empty

  def getClazzById(classId: ClassId): Clazz = classByIds(classId)

  private def registerClazz[A <: Clazz](c: Int => A): A = {
    val clazz = c(classByIds.size)
    classByIds += clazz
    clazz
  }

  private def loadClazz(className: String): InstanceClazz = {
    val classBytes: Array[Byte] = providers.find(
      _.containsClass(className)
    ).getOrElse(throw FClassLoader.ClassProviderNotFound(className))
      .getClass(className)
    ClassFileInfo.fromBytes(classBytes) match {
      case Validated.Valid(byteCode: ClassFileInfo) =>
        // Resolved before classId is captured: superclass/interface resolution can recursively
        // load and register other classes, so classByIds.size must be read *after* that
        // settles, or this class's classId would drift from its actual insertion index.
        val superClass = byteCode.superClassName.map(getInstanceClass)
        val interfaces = byteCode.interfaceNames.map(getInstanceClass)
        val superAllFields = superClass.map(_.allFields).getOrElse(Map.empty)
        registerClazz(byteCode.buildClazz(heap, _, superClass, interfaces, superAllFields))
      case Validated.Invalid(e) =>
        throw new Exception(s"Unable to read bytecode for $className: ${e.toList.mkString(" ")}")
    }
  }

  private def loadArrayClazz(name: String): ArrayClazz = {
    assert(name.head == '[')
    val innerTypeName = name.tail
    val innerType = innerTypeName.head match
      case 'L' =>
        val className = innerTypeName.tail.init
        getClass(className)
        FType.parseOne(innerTypeName)._1
      case '[' =>
        val className = innerTypeName.tail
        getClass(className)
        FType.parseOne(innerTypeName)._1
      case x => FType.parseOne(innerTypeName)._1
    registerClazz(ArrayClazz(name, innerType, heap, _))
  }

  private def initClass(clazz: InstanceClazz): Unit = {
    val objectClazz = if (clazz.name == WKObject.className) clazz else getInstanceClass(WKObject.className)

    def runAndCheck(meth: JvmMethod): Unit =
      BytecodeExecutor(meth, clazz, this, heap, objectClazz, nativeMethodCatalog, BytecodeExecutor.sinkReturn)
        .run() match {
        case BytecodeExecutor.FThrowableOutcome(ref, throwable) =>
          throw BytecodeExecutor.UncaughtFThrowable(ref, throwable)
        case BytecodeExecutor.FReturnValueOutcome(_) => ()
      }

    clazz.maybeClinitMet.foreach(runAndCheck)
    if (clazz.name == "java/lang/System") {
      val initMeth = clazz.jvmMethods.filter(_._1._1 == "initializeSystemClass").head._2
      runAndCheck(initMeth)
    }
  }

  private def resolveClass(className: String, init: Boolean): Clazz = {
    className.head match
      case '[' =>
        arrayClazzs.get(className) match {
          case Some(c) => c
          case None =>
            val arrayClazz = loadArrayClazz(className)
            arrayClazzs(className) = arrayClazz
            arrayClazz
        }
      case _ =>
        instancesClazzs.get(className) match {
          case Some(c) => c
          case None =>
            val instanceClazzs = loadClazz(className)
            instancesClazzs(className) = instanceClazzs
            if (init) initClass(instanceClazzs)
            instanceClazzs
        }
  }

  def getClass(className: String): Clazz = resolveClass(className, init = true)

  def getWithoutInit(className: String): Clazz = resolveClass(className, init = false)

  def getInstanceClass(className: String): InstanceClazz = {
    getClass(className) match {
      case c: InstanceClazz => c
      case _ => throw new IllegalArgumentException("Expecting instance class")
    }
  }

  def getArrayClass(className: String): ArrayClazz = {
    getClass(className) match {
      case c: ArrayClazz => c
      case _ => throw new IllegalArgumentException("Expecting array class")
    }
  }

  private var primitiveClass: Option[Map[String, Heap.Address]] = None

  def primitiveClass(name: String): Heap.Address =
    primitiveClass.getOrElse(throw new RuntimeException("Primitive class must be init"))(name)

  def initPrimitiveClass(): Unit = {
    val clazz = this.getInstanceClass(WKClass.className)
    val res = Utils.primitiveNames.map { name =>
      name -> heap.new_(clazz)
    }.toMap
    primitiveClass = Some(res)
  }

  def isPrimitiveClassAddr(addr: Heap.Address): Boolean =
    primitiveClass.exists(_.values.toSet.contains(addr))

}
