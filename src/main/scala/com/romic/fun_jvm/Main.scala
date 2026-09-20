package com.romic.fun_jvm

import com.romic.fun_jvm.Clazz.MethodDescriptor

import java.nio.file.Path

@main def hello(classFilePath: String): Unit = {
  // target/scala-3.9.0/classes/JVMarch/Main.class
//  val classLoader2 = new ClassLoader(Path.of(classFilePath))
  val classLoader = ClassLoaderBuilder()
    .withClassFileDir(Path.of(classFilePath))
    .withJar(Path.of(s"/home/rmichau/.sdkman/candidates/java/8.0.442-zulu/jre/lib/rt.jar"))
    .build()
  val mainClass = classLoader.getClass("JVMarch/Main")
  //  val bytes = ClassLoader.getPlatformClassLoader
  //    .getResourceAsStream("java/nio/file/Path.class")
  //    .readAllBytes()
  //  val yo = ClassFile.fromBytes(bytes)
  println(mainClass.jvmMethods.keys)
  val main = mainClass.jvmMethods(("main", MethodDescriptor.parseMethodDescriptor("([Ljava/lang/String;)V")))
  (FunctionInterpreter(main, mainClass, classLoader)).run()
}