object OpCode {
  private def b(i: Int): Byte = (i & 0xff).toByte
  val iconst_m1: Byte = b(0x02)
  val iconst_0: Byte = b(0x03)
  val iconst_1: Byte = b(0x04)
  val iconst_2: Byte = b(0x05)
  val iconst_3: Byte = b(0x06)
  val iconst_4: Byte = b(0x07)
  val iconst_5: Byte = b(0x08)

  val istore_0: Byte = b(0x3b)
  val istore_1: Byte = b(0x3c)
  val istore_2: Byte = b(0x3d)
  val istore_3: Byte = b(0x3e)

  val ldc2_w: Byte = b(0x14)

  val return_ : Byte =  b(0xb1)
}
