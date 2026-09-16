@main def hello(): Unit = {
  val yo = ClassFile.load("/Users/r.michau/perso/fun_jvm/target/scala-3.9.0/classes/JVMarch/Main.class")
  println(yo)
}

