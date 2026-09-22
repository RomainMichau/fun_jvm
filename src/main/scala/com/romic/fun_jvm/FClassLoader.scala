package com.romic.fun_jvm

import cats.data.Validated
import com.romic.fun_jvm.classparser.ClassFileInfo

import java.nio.file.{Files, Path, Paths}
import java.util.zip.{ZipEntry, ZipFile}
import scala.jdk.CollectionConverters
import scala.collection.mutable
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

class FClassLoader(providers: List[ClassBytesProvider], heap: Heap, nativeMethodCatalog: NativeMethodCatalog) {
  private val clazzs: mutable.Map[String, Clazz] = mutable.Map.empty

  private def loadClazz(className: String): Clazz = {
    val classBytes: Array[Byte] = providers.find(
      _.containsClass(className)
    ).getOrElse(throw new Exception(s"No class provider contains $className"))
      .getClass(className)
    val clazz = ClassFileInfo.fromBytes(classBytes) match {
      case Validated.Valid(byteCode) => byteCode.initClass(heap)
      case Validated.Invalid(e) =>
        throw new Exception(s"Unable to read bytecode for $className: ${e.toList.mkString(" ")}")
    }

    clazz
  }

  private def initClass(clazz: Clazz): Unit = {
    // this trigger init of super class
    clazz.resolveSuperAndInterfaces(this)
    val meth: Unit =
      clazz.maybeClinitMet.foreach(meth => BytecodeExecutor(meth, clazz, this, heap, null, nativeMethodCatalog).run())
    if (clazz.name == "java/lang/System") {
      val initMeth = clazz.jvmMethods.filter(_._1._1 == "initializeSystemClass").head._2
      BytecodeExecutor(initMeth, clazz, this, heap, null, nativeMethodCatalog).run()
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

}
