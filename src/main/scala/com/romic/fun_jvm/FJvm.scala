package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.MethodDescriptor
import com.romic.fun_jvm.classloader.{ClassLoaderBuilder, FClassLoader}

import java.nio.file.Path

object FJvm {

  private def genesis(heap: Heap, classLoader: FClassLoader): Clazz = {
    val classToPreloads = Set("java/lang/System", "java/lang/Object", "java/lang/String")
    val loadedClazz = classToPreloads.map(x => classLoader.getClass(x))
    loadedClazz.find(_.name == "java/lang/Object").head
  }

  def main(args: Array[String]): Unit = {

    // target/scala-3.9.0/classes/JVMarch/Main.class
    val classFilePath = args.headOption.getOrElse {
      System.err.println("usage: hello <classFileDir>")
      sys.exit(1)
    }
    val heap = new Heap(3000)

    val nativeMethodCatalog = new NativeMethodCatalog(heap)

    val classLoader = ClassLoaderBuilder()
      .withClassFileDir(Path.of(classFilePath))
      .withJar(Path.of(s"/home/rmichau/.sdkman/candidates/java/8.0.442-zulu/jre/lib/rt.jar"))
      .build(heap, nativeMethodCatalog)

    classLoader.initPrimitiveClass()

    val objectClazz = genesis(heap, classLoader)
    val mainClass = classLoader.getClass("JVMarch/Main")
    //  val bytes = ClassLoader.getPlatformClassLoader
    //    .getResourceAsStream("java/nio/file/Path.class")
    //    .readAllBytes()
    //  val yo = ClassFile.fromBytes(bytes)
    println(mainClass.jvmMethods.keys)
    val main = mainClass.jvmMethods(("main", MethodDescriptor.parseMethodDescriptor("([Ljava/lang/String;)V")))
    BytecodeExecutor(main, mainClass, classLoader, heap, objectClazz, nativeMethodCatalog).run()
  }

}
