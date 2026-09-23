package com.romic.fun_jvm.classloader

import cats.data.Validated
import com.romic.fun_jvm.classparser.ClassFileInfo
import com.romic.fun_jvm.*
import com.romic.fun_jvm.utils.Utils
import com.romic.fun_jvm.well_known.WKClass

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
}

class FClassLoader(providers: List[ClassBytesProvider], heap: Heap, nativeMethodCatalog: NativeMethodCatalog) {
  private val clazzs: mutable.Map[String, Clazz] = mutable.Map.empty

  private val classByIds: mutable.ArrayBuffer[Clazz] = mutable.ArrayBuffer.empty

  private def loadClazz(className: String): Clazz = {
    val classBytes: Array[Byte] = providers.find(
      _.containsClass(className)
    ).getOrElse(throw new Exception(s"No class provider contains $className"))
      .getClass(className)
    ClassFileInfo.fromBytes(classBytes) match {
      case Validated.Valid(byteCode: ClassFileInfo) =>
        // Resolved before classId is captured: superclass/interface resolution can recursively
        // load and register other classes, so classByIds.size must be read *after* that
        // settles, or this class's classId would drift from its actual insertion index.
        val superClass = byteCode.superClassName.map(getClass)
        val interfaces = byteCode.interfaceNames.map(getClass)
        val classId = classByIds.size
        val superFields = superClass.map(_.directInstanceFields).getOrElse(Map.empty)
        val clazz = byteCode.buildClazz(heap, classId, superClass, interfaces, superFields)
        classByIds += clazz
        clazz
      case Validated.Invalid(e) =>
        throw new Exception(s"Unable to read bytecode for $className: ${e.toList.mkString(" ")}")
    }
  }

  private def initClass(clazz: Clazz): Unit = {
    val meth: Unit =
      clazz.maybeClinitMet.foreach(meth =>
        BytecodeExecutor(meth, clazz, this, heap, null, nativeMethodCatalog, BytecodeExecutor.sinkReturn).run()
      )
    if (clazz.name == "java/lang/System") {
      val initMeth = clazz.jvmMethods.filter(_._1._1 == "initializeSystemClass").head._2
      BytecodeExecutor(initMeth, clazz, this, heap, null, nativeMethodCatalog, BytecodeExecutor.sinkReturn).run()
    }
  }

  def getClass(className: String): Clazz = {
    clazzs.get(className) match {
      case Some(c) => c
      case None =>
        val clazz = loadClazz(className)
        clazzs(className) = clazz
        initClass(clazz)
        clazz

    }
  }

  private var primitiveClass: Option[Map[String, Heap.Address]] = None

  def primitiveClass(name: String): Heap.Address =
    primitiveClass.getOrElse(throw new RuntimeException("Primitive class must be init"))(name)

  def initPrimitiveClass(): Unit = {
    val clazz = this.getClass(WKClass.className)
    val res = Utils.primitiveNames.map { name =>
      name -> heap.storeNew(clazz)
    }.toMap
    primitiveClass = Some(res)
  }

}
