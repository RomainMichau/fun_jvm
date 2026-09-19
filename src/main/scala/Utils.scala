object Utils {
  case class ByteRange(from: Byte, to: Byte) {
    def contains(b: Byte): Boolean = b >= from && b <= to
  }
  
  extension (b: Byte) {
    def to(b2: Byte): ByteRange = ByteRange(b, b2)
  }
  
}
