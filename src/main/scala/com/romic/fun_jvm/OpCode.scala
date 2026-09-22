package com.romic.fun_jvm

/**
 * @param operandBytes number of bytes following the opcode itself in the bytecode stream.
 *                     -1 marks variable-length instructions (tableswitch, lookupswitch, wide),
 *                     whose actual operand size can't be known without also reading the operands.
 */
enum OpCode(val op: Byte, val operandBytes: Byte):
  private def this(code: Int, operandBytes: Int) = this((code & 0xff).toByte, operandBytes.toByte)

  case nop extends OpCode(0x00, 0)
  case aconst_null extends OpCode(0x01, 0)
  case iconst_m1 extends OpCode(0x02, 0)
  case iconst_0 extends OpCode(0x03, 0)
  case iconst_1 extends OpCode(0x04, 0)
  case iconst_2 extends OpCode(0x05, 0)
  case iconst_3 extends OpCode(0x06, 0)
  case iconst_4 extends OpCode(0x07, 0)
  case iconst_5 extends OpCode(0x08, 0)
  case lconst_0 extends OpCode(0x09, 0)
  case lconst_1 extends OpCode(0x0a, 0)
  case fconst_0 extends OpCode(0x0b, 0)
  case fconst_1 extends OpCode(0x0c, 0)
  case fconst_2 extends OpCode(0x0d, 0)
  case dconst_0 extends OpCode(0x0e, 0)
  case dconst_1 extends OpCode(0x0f, 0)

  case bipush extends OpCode(0x10, 1)
  case sipush extends OpCode(0x11, 2)
  case ldc extends OpCode(0x12, 1)
  case ldc_w extends OpCode(0x13, 2)
  case ldc2_w extends OpCode(0x14, 2)

  case iload extends OpCode(0x15, 1)
  case lload extends OpCode(0x16, 1)
  case fload extends OpCode(0x17, 1)
  case dload extends OpCode(0x18, 1)
  case aload extends OpCode(0x19, 1)
  case iload_0 extends OpCode(0x1a, 0)
  case iload_1 extends OpCode(0x1b, 0)
  case iload_2 extends OpCode(0x1c, 0)
  case iload_3 extends OpCode(0x1d, 0)
  case lload_0 extends OpCode(0x1e, 0)
  case lload_1 extends OpCode(0x1f, 0)
  case lload_2 extends OpCode(0x20, 0)
  case lload_3 extends OpCode(0x21, 0)
  case fload_0 extends OpCode(0x22, 0)
  case fload_1 extends OpCode(0x23, 0)
  case fload_2 extends OpCode(0x24, 0)
  case fload_3 extends OpCode(0x25, 0)
  case dload_0 extends OpCode(0x26, 0)
  case dload_1 extends OpCode(0x27, 0)
  case dload_2 extends OpCode(0x28, 0)
  case dload_3 extends OpCode(0x29, 0)
  case aload_0 extends OpCode(0x2a, 0)
  case aload_1 extends OpCode(0x2b, 0)
  case aload_2 extends OpCode(0x2c, 0)
  case aload_3 extends OpCode(0x2d, 0)

  case iaload extends OpCode(0x2e, 0)
  case laload extends OpCode(0x2f, 0)
  case faload extends OpCode(0x30, 0)
  case daload extends OpCode(0x31, 0)
  case aaload extends OpCode(0x32, 0)
  case baload extends OpCode(0x33, 0)
  case caload extends OpCode(0x34, 0)
  case saload extends OpCode(0x35, 0)

  case istore extends OpCode(0x36, 1)
  case lstore extends OpCode(0x37, 1)
  case fstore extends OpCode(0x38, 1)
  case dstore extends OpCode(0x39, 1)
  case astore extends OpCode(0x3a, 1)
  case istore_0 extends OpCode(0x3b, 0)
  case istore_1 extends OpCode(0x3c, 0)
  case istore_2 extends OpCode(0x3d, 0)
  case istore_3 extends OpCode(0x3e, 0)
  case lstore_0 extends OpCode(0x3f, 0)
  case lstore_1 extends OpCode(0x40, 0)
  case lstore_2 extends OpCode(0x41, 0)
  case lstore_3 extends OpCode(0x42, 0)
  case fstore_0 extends OpCode(0x43, 0)
  case fstore_1 extends OpCode(0x44, 0)
  case fstore_2 extends OpCode(0x45, 0)
  case fstore_3 extends OpCode(0x46, 0)
  case dstore_0 extends OpCode(0x47, 0)
  case dstore_1 extends OpCode(0x48, 0)
  case dstore_2 extends OpCode(0x49, 0)
  case dstore_3 extends OpCode(0x4a, 0)
  case astore_0 extends OpCode(0x4b, 0)
  case astore_1 extends OpCode(0x4c, 0)
  case astore_2 extends OpCode(0x4d, 0)
  case astore_3 extends OpCode(0x4e, 0)

  case iastore extends OpCode(0x4f, 0)
  case lastore extends OpCode(0x50, 0)
  case fastore extends OpCode(0x51, 0)
  case dastore extends OpCode(0x52, 0)
  case aastore extends OpCode(0x53, 0)
  case bastore extends OpCode(0x54, 0)
  case castore extends OpCode(0x55, 0)
  case sastore extends OpCode(0x56, 0)

  case pop extends OpCode(0x57, 0)
  case pop2 extends OpCode(0x58, 0)
  case dup extends OpCode(0x59, 0)
  case dup_x1 extends OpCode(0x5a, 0)
  case dup_x2 extends OpCode(0x5b, 0)
  case dup2 extends OpCode(0x5c, 0)
  case dup2_x1 extends OpCode(0x5d, 0)
  case dup2_x2 extends OpCode(0x5e, 0)
  case swap extends OpCode(0x5f, 0)

  case iadd extends OpCode(0x60, 0)
  case ladd extends OpCode(0x61, 0)
  case fadd extends OpCode(0x62, 0)
  case dadd extends OpCode(0x63, 0)
  case isub extends OpCode(0x64, 0)
  case lsub extends OpCode(0x65, 0)
  case fsub extends OpCode(0x66, 0)
  case dsub extends OpCode(0x67, 0)
  case imul extends OpCode(0x68, 0)
  case lmul extends OpCode(0x69, 0)
  case fmul extends OpCode(0x6a, 0)
  case dmul extends OpCode(0x6b, 0)
  case idiv extends OpCode(0x6c, 0)
  case ldiv extends OpCode(0x6d, 0)
  case fdiv extends OpCode(0x6e, 0)
  case ddiv extends OpCode(0x6f, 0)
  case irem extends OpCode(0x70, 0)
  case lrem extends OpCode(0x71, 0)
  case frem extends OpCode(0x72, 0)
  case drem extends OpCode(0x73, 0)
  case ineg extends OpCode(0x74, 0)
  case lneg extends OpCode(0x75, 0)
  case fneg extends OpCode(0x76, 0)
  case dneg extends OpCode(0x77, 0)
  case ishl extends OpCode(0x78, 0)
  case lshl extends OpCode(0x79, 0)
  case ishr extends OpCode(0x7a, 0)
  case lshr extends OpCode(0x7b, 0)
  case iushr extends OpCode(0x7c, 0)
  case lushr extends OpCode(0x7d, 0)
  case iand extends OpCode(0x7e, 0)
  case land extends OpCode(0x7f, 0)
  case ior extends OpCode(0x80, 0)
  case lor extends OpCode(0x81, 0)
  case ixor extends OpCode(0x82, 0)
  case lxor extends OpCode(0x83, 0)
  case iinc extends OpCode(0x84, 2)

  case i2l extends OpCode(0x85, 0)
  case i2f extends OpCode(0x86, 0)
  case i2d extends OpCode(0x87, 0)
  case l2i extends OpCode(0x88, 0)
  case l2f extends OpCode(0x89, 0)
  case l2d extends OpCode(0x8a, 0)
  case f2i extends OpCode(0x8b, 0)
  case f2l extends OpCode(0x8c, 0)
  case f2d extends OpCode(0x8d, 0)
  case d2i extends OpCode(0x8e, 0)
  case d2l extends OpCode(0x8f, 0)
  case d2f extends OpCode(0x90, 0)
  case i2b extends OpCode(0x91, 0)
  case i2c extends OpCode(0x92, 0)
  case i2s extends OpCode(0x93, 0)

  case lcmp extends OpCode(0x94, 0)
  case fcmpl extends OpCode(0x95, 0)
  case fcmpg extends OpCode(0x96, 0)
  case dcmpl extends OpCode(0x97, 0)
  case dcmpg extends OpCode(0x98, 0)

  case ifeq extends OpCode(0x99, 2)
  case ifne extends OpCode(0x9a, 2)
  case iflt extends OpCode(0x9b, 2)
  case ifge extends OpCode(0x9c, 2)
  case ifgt extends OpCode(0x9d, 2)
  case ifle extends OpCode(0x9e, 2)
  case if_icmpeq extends OpCode(0x9f, 2)
  case if_icmpne extends OpCode(0xa0, 2)
  case if_icmplt extends OpCode(0xa1, 2)
  case if_icmpge extends OpCode(0xa2, 2)
  case if_icmpgt extends OpCode(0xa3, 2)
  case if_icmple extends OpCode(0xa4, 2)
  case if_acmpeq extends OpCode(0xa5, 2)
  case if_acmpne extends OpCode(0xa6, 2)

  case goto extends OpCode(0xa7, 2)
  case jsr extends OpCode(0xa8, 2)
  case ret extends OpCode(0xa9, 1)
  case tableswitch extends OpCode(0xaa, -1)
  case lookupswitch extends OpCode(0xab, -1)

  case ireturn extends OpCode(0xac, 0)
  case lreturn extends OpCode(0xad, 0)
  case freturn extends OpCode(0xae, 0)
  case dreturn extends OpCode(0xaf, 0)
  case areturn extends OpCode(0xb0, 0)
  case return_ extends OpCode(0xb1, 0)

  case getstatic extends OpCode(0xb2, 2)
  case putstatic extends OpCode(0xb3, 2)
  case getfield extends OpCode(0xb4, 2)
  case putfield extends OpCode(0xb5, 2)

  case invokevirtual extends OpCode(0xb6, 2)
  case invokespecial extends OpCode(0xb7, 2)
  case invokestatic extends OpCode(0xb8, 2)
  case invokeinterface extends OpCode(0xb9, 4)
  case invokedynamic extends OpCode(0xba, 4)

  case new_ extends OpCode(0xbb, 2)
  case newarray extends OpCode(0xbc, 1)
  case anewarray extends OpCode(0xbd, 2)
  case arraylength extends OpCode(0xbe, 0)
  case athrow extends OpCode(0xbf, 0)
  case checkcast extends OpCode(0xc0, 2)
  case instanceof extends OpCode(0xc1, 2)
  case monitorenter extends OpCode(0xc2, 0)
  case monitorexit extends OpCode(0xc3, 0)

  case wide extends OpCode(0xc4, -1)
  case multianewarray extends OpCode(0xc5, 3)
  case ifnull extends OpCode(0xc6, 2)
  case ifnonnull extends OpCode(0xc7, 2)
  case goto_w extends OpCode(0xc8, 4)
  case jsr_w extends OpCode(0xc9, 4)

  // Reserved opcodes, not generated by any legitimate compiler
  case breakpoint extends OpCode(0xca, 0)
  case impdep1 extends OpCode(0xfe, 0)
  case impdep2 extends OpCode(0xff, 0)

object OpCode {
  private val byOp: Map[Byte, OpCode] = values.map(v => v.op -> v).toMap

  def fromByte(b: Byte): Option[OpCode] = byOp.get(b)

  def nameOf(b: Byte): String = fromByte(b).map(_.toString).getOrElse(f"unknown(0x${b & 0xff}%02x)")

  def codesToString(bytes: Vector[Byte]): String = {
    if (bytes.isEmpty) ""
    else {

      val opCode = fromByte(bytes.head).get
      val operandByteCount = opCode.operandBytes
      if operandByteCount == -1 then ???
      val operandsSt = bytes.slice(1, 1 + operandByteCount).mkString("(", ",", ")")
      val res = s"${opCode.toString}${operandsSt}"
      s"$res ${codesToString(bytes.drop(1 + operandByteCount))}"
    }
  }

  def codeToString(bytes: Vector[Byte]): String = {
    if (bytes.isEmpty) ""
    else {
      val opCode = fromByte(bytes.head).get
      val operandByteCount = opCode.operandBytes
      if operandByteCount == -1 then ???
      val operandsSt = bytes.slice(1, 1 + operandByteCount).mkString("(", ",", ")")
      val res = s"${opCode.toString}${operandsSt}"
      res
    }
  }

}
