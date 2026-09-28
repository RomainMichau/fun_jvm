package com.romic.fun_jvm

import com.romic.fun_jvm.InstanceClazz.MethodDescriptor
import com.romic.fun_jvm.classloader.{ClassLoaderBuilder, FClassLoader}
import com.romic.fun_jvm.well_known.WKThrowable

import java.nio.file.Path

object FJvm {

  private def genesis(heap: Heap, classLoader: FClassLoader): InstanceClazz = {
    val classToPreloads = Set("java/lang/System", "java/lang/Object", "java/lang/String")
    val loadedClazz = classToPreloads.map(x => classLoader.getInstanceClass(x))
    loadedClazz.find(_.name == "java/lang/Object").head
  }

  private val defaultTestJar = "target/jvmarch-test.jar"

  def main(args: Array[String]): Unit = {

    val classFilePath = args.headOption.getOrElse(defaultTestJar)
    val heap = new Heap(30000000)

    val nativeMethodCatalog = new NativeMethodCatalog(heap)

    val classLoader = ClassLoaderBuilder()
      .withJar(Path.of(classFilePath))
      .withJar(Path.of(s"/home/rmichau/.sdkman/candidates/java/8.0.442-zulu/jre/lib/rt.jar"))
      .build(heap, nativeMethodCatalog)

    try {
      classLoader.initPrimitiveClass()

      val objectClazz = genesis(heap, classLoader)
      val mainClass = classLoader.getInstanceClass("JVMarch/Main")
      //  val bytes = ClassLoader.getPlatformClassLoader
      //    .getResourceAsStream("java/nio/file/Path.class")
      //    .readAllBytes()
      //  val yo = ClassFile.fromBytes(bytes)
      println(mainClass.jvmMethods.keys)
      val main = mainClass.jvmMethods(("main", MethodDescriptor.parseMethodDescriptor("([Ljava/lang/String;)V")))
      BytecodeExecutor(
        main,
        mainClass,
        classLoader,
        heap,
        objectClazz,
        nativeMethodCatalog,
        BytecodeExecutor.sinkReturn
      ).run() match {
        case BytecodeExecutor.FThrowableOutcome(ref, throwable) =>
          throw BytecodeExecutor.UncaughtFThrowable(ref, throwable)
        case BytecodeExecutor.FReturnValueOutcome(_) => ()
      }
    } catch {
      case BytecodeExecutor.UncaughtFThrowable(ref, throwable) =>
        System.err.println(WKThrowable.uncaughtMessage(ref, throwable, heap, classLoader))
        sys.exit(1)
    }
  }

}
