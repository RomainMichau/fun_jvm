package com.romic.fun_jvm

import com.romic.fun_jvm.InstanceClazz.MethodDescriptor
import com.romic.fun_jvm.classloader.{ClassLoaderBuilder, FClassLoader}
import com.romic.fun_jvm.well_known.WKThrowable
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

object FJvm {
  private val logger = LoggerFactory.getLogger(getClass)

  private def genesis(heap: Heap, classLoader: FClassLoader): InstanceClazz = {
    val classToPreloads = Set("java/lang/System", "java/lang/Object", "java/lang/String")
    val loadedClazz = classToPreloads.map(x => classLoader.getInstanceClass(x))
    loadedClazz.find(_.name == "java/lang/Object").head
  }

  private val defaultTestJar = "target/jvmarch-test.jar"
  private val defaultRtJar = Path.of("/home/rmichau/.sdkman/candidates/java/8.0.442-zulu/jre/lib/rt.jar")

  private def java8RtJar: Path = {
    val sdkmanDirectories = List(
      Option(System.getenv("SDKMAN_DIR")).map(Path.of(_)),
      Some(Path.of(System.getProperty("user.home"), ".sdkman")),
      Some(Path.of(System.getProperty("user.home"), "devtools", "sdkman"))
    ).flatten

    val sdkmanRtJars = sdkmanDirectories.flatMap { sdkmanDirectory =>
      val javaCandidates = sdkmanDirectory.resolve("candidates/java")
      if (Files.isDirectory(javaCandidates)) {
        Using.resource(Files.list(javaCandidates))(_.iterator.asScala.toList)
          .filter(path => path.getFileName.toString.startsWith("8."))
          .flatMap { candidate =>
            List(
              candidate.resolve("jre/lib/rt.jar"),
              candidate.resolve("zulu-8.jdk/Contents/Home/jre/lib/rt.jar")
            )
          }
      } else List.empty
    }

    (defaultRtJar :: sdkmanRtJars).find(path => Files.isRegularFile(path)).getOrElse(defaultRtJar)
  }

  def main(args: Array[String]): Unit = {

    val classFilePath = args.headOption.getOrElse(defaultTestJar)
    val heap = new Heap(30000000)

    val nativeMethodCatalog = new NativeMethodCatalog(heap)

    val classLoader = ClassLoaderBuilder()
      .withJar(Path.of(classFilePath))
      .withJar(java8RtJar)
      .build(heap, nativeMethodCatalog)

    try {
      classLoader.initPrimitiveClass()

      val objectClazz = genesis(heap, classLoader)
      logger.info("JVM warmup complete")
      val mainClass = classLoader.getInstanceClass("JVMarch/Main")
      logger.debug("Loaded main class methods: {}", mainClass.jvmMethods.keys)
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
        logger.error(WKThrowable.uncaughtMessage(ref, throwable, heap, classLoader))
        sys.exit(1)
    }
  }

}
