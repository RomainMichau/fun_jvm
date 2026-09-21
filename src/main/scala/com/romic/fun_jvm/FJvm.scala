package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.MethodDescriptor

import java.nio.file.Path

object FJvm {

  private def genesis(heap: Heap, classLoader: ClassLoader): Unit = {
    val classToPreloads = Set("java/lang/System", "java/lang/Object", "java/lang/String")
    classToPreloads.foreach(x => classLoader.getClass(x))
  }
  def main(args: Array[String]): Unit = {

    // target/scala-3.9.0/classes/JVMarch/Main.class
    val classFilePath = args.headOption.getOrElse {
      System.err.println("usage: hello <classFileDir>")
      sys.exit(1)
    }
    val heap = new Heap(3000)

    val classLoader = ClassLoaderBuilder()
      .withClassFileDir(Path.of(classFilePath))
      .withJar(Path.of(s"/home/rmichau/.sdkman/candidates/java/8.0.442-zulu/jre/lib/rt.jar"))
      .build(heap)

    genesis(heap, classLoader)
    val mainClass = classLoader.getClass("JVMarch/Main")
    //  val bytes = ClassLoader.getPlatformClassLoader
    //    .getResourceAsStream("java/nio/file/Path.class")
    //    .readAllBytes()
    //  val yo = ClassFile.fromBytes(bytes)
    println(mainClass.jvmMethods.keys)
    val main = mainClass.jvmMethods(("main", MethodDescriptor.parseMethodDescriptor("([Ljava/lang/String;)V")))
    (FunctionInterpreter(main, mainClass, classLoader, heap)).run()
  }
}
