package com.romic.fun_jvm

import java.io.{ByteArrayInputStream, DataInputStream}

// JVM class files encode CONSTANT_Utf8_info strings as "modified UTF-8" (spec 4.4.7):
// same as UTF-8 except the null char is 2 bytes (0xC0 0x80) instead of 1, and
// supplementary chars are 2 separate 3-byte surrogate halves instead of one 4-byte
// sequence. That's exactly the format java.io.DataInputStream.readUTF()/writeUTF()
// use, so delegate to it instead of hand-rolling the decoder.
object ModifiedUtf8Decoder {

  // `offset` must point at the u2 length field (i.e. right after the tag byte);
  // `length` is that already-parsed value (the number of following string bytes).
  def decode(bytes: Array[Byte], offset: Int, length: Int): String =
    new DataInputStream(new ByteArrayInputStream(bytes, offset, 2 + length)).readUTF()

}
