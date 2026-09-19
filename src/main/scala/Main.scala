@main def hello(): Unit = {
  val yo: ClassFileInfo = ClassFileInfo.load("/Users/r.michau/perso/fun_jvm/target/scala-3.9.0/classes/JVMarch/Main.class").toOption.get
  //  val bytes = ClassLoader.getPlatformClassLoader
  //    .getResourceAsStream("java/nio/file/Path.class")
  //    .readAllBytes()
  //  val yo = ClassFile.fromBytes(bytes)
  println(yo.methods.keys)
  val main = yo.methods(("main", "([Ljava/lang/String;)V"))
  (Interpreter(main.code, yo.constantPool)).run()
}

