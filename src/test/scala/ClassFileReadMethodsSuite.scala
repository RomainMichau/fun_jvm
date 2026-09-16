import cats.data.Validated.{Invalid, Valid}

class ClassFileReadMethodsSuite extends munit.FunSuite {

  test("read2Bytes reads two bytes big-endian into a UShort") {
    val bytes = Array[Byte](0x01, 0x02)
    assertEquals(ClassFile.read2Bytes(bytes, 0).map(_.toInt), Valid(0x0102))
  }

  test("read2Bytes handles the full unsigned range (no sign issues)") {
    val bytes = Array[Byte](0xff.toByte, 0xff.toByte)
    assertEquals(ClassFile.read2Bytes(bytes, 0).map(_.toInt), Valid(65535))
  }

  test("read2Bytes reads at a non-zero offset") {
    val bytes = Array[Byte](0x00, 0x00, 0x12, 0x34)
    assertEquals(ClassFile.read2Bytes(bytes, 2).map(_.toInt), Valid(0x1234))
  }

  test("read2Bytes fails with the given label when the array is too short") {
    val bytes = Array[Byte](0x01)
    ClassFile.read2Bytes(bytes, 0, "custom label") match {
      case Invalid(errors) => assertEquals(errors.toList, List("Unable to get custom label"))
      case Valid(v) => fail(s"expected Invalid, got Valid($v)")
    }
  }

  test("readInt reads four bytes big-endian") {
    val bytes = Array[Byte](0x00, 0x00, 0x00, 0x01)
    assertEquals(ClassFile.readInt(bytes, 0), Valid(1))
  }

  test("readInt reconstructs a negative two's-complement value") {
    val bytes = Array[Byte](0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    assertEquals(ClassFile.readInt(bytes, 0), Valid(-1))
  }

  test("readInt fails when the array is too short") {
    val bytes = Array[Byte](0x00, 0x00, 0x00)
    assert(ClassFile.readInt(bytes, 0).isInvalid)
  }

  test("readFloat decodes 0.0") {
    val bytes = Array[Byte](0x00, 0x00, 0x00, 0x00)
    assertEquals(ClassFile.readFloat(bytes, 0), Valid(0.0f))
  }

  test("readFloat decodes 1.0") {
    val bytes = Array(0x3f, 0x80, 0x00, 0x00).map(_.toByte)
    assertEquals(ClassFile.readFloat(bytes, 0), Valid(1.0f))
  }

  test("readFloat decodes -1.0 (sign bit applied correctly)") {
    val bytes = Array(0xbf, 0x80, 0x00, 0x00).map(_.toByte)
    assertEquals(ClassFile.readFloat(bytes, 0), Valid(-1.0f))
  }

  test("readFloat decodes positive infinity") {
    val bytes = Array(0x7f, 0x80, 0x00, 0x00).map(_.toByte)
    assertEquals(ClassFile.readFloat(bytes, 0), Valid(Float.PositiveInfinity))
  }

  test("readFloat decodes negative infinity") {
    val bytes = Array(0xff, 0x80, 0x00, 0x00).map(_.toByte)
    assertEquals(ClassFile.readFloat(bytes, 0), Valid(Float.NegativeInfinity))
  }

  test("readFloat decodes NaN") {
    val bytes = Array(0x7f, 0xc0, 0x00, 0x00).map(_.toByte)
    assert(ClassFile.readFloat(bytes, 0).exists(_.isNaN))
  }

  test("readFloat fails when the array is too short") {
    val bytes = Array[Byte](0x00, 0x00, 0x00)
    assert(ClassFile.readFloat(bytes, 0).isInvalid)
  }

  // Reference value computed independently via java.nio.ByteBuffer, so expectations here
  // don't depend on hand-doing the same bit arithmetic readLong itself performs.
  private def bigEndianLong(bytes: Array[Byte]): Long = java.nio.ByteBuffer.wrap(bytes).getLong()

  test("readLong reads eight bytes big-endian") {
    val bytes = Array[Byte](0, 0, 0, 0, 0, 0, 0, 1)
    assertEquals(ClassFile.readLong(bytes, 0), Valid(bigEndianLong(bytes)))
  }

  test("readLong reads a value in the low bytes") {
    val bytes = Array[Byte](0, 0, 0, 0, 0, 0, 1, 0)
    assertEquals(ClassFile.readLong(bytes, 0), Valid(bigEndianLong(bytes)))
  }

  test("readLong reads a value at a non-zero offset") {
    val prefix = Array[Byte](0x11, 0x22)
    val payload = Array[Byte](0, 0, 0, 0, 0, 0, 1, 0)
    assertEquals(ClassFile.readLong(prefix ++ payload, 2), Valid(bigEndianLong(payload)))
  }

  test("readLong reconstructs -1L from all-0xFF bytes") {
    val bytes = Array.fill[Byte](8)(0xff.toByte)
    assertEquals(ClassFile.readLong(bytes, 0), Valid(bigEndianLong(bytes)))
  }

  // KNOWN BUG: only byte `a` is widened to Long (`a.toLong & 0xff`) before shifting; bytes
  // b..h are still shifted as Int. Shifts >=32 (on b, c, d) wrap via Int's mod-32 shift
  // masking, and any of b..h with its own high bit set (>= 0x80) produces a negative Int
  // that gets sign-extended to Long on the `|`, corrupting bits far outside that byte's
  // own position. This reproduces the second failure mode: byte `e` (bit 24) set high.
  test("readLong on a high-bit byte away from the edges (currently broken)".fail) {
    val bytes = Array[Byte](0, 0, 0, 0, 0xff.toByte, 0, 0, 0)
    assertEquals(ClassFile.readLong(bytes, 0), Valid(bigEndianLong(bytes)))
  }

  test("readLong fails when the array is too short") {
    val bytes = Array[Byte](0, 0, 0, 0, 0, 0, 0)
    assert(ClassFile.readLong(bytes, 0).isInvalid)
  }
}
