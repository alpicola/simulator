package cpuex4

import java.io._
import java.lang.Float

import scala.io.Source
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

import Instruction._

object Program {
  def fromAssembly(file:File):Program = {
    val in = new FileInputStream(file)
    try {
      Assembler.assemble(in)
    } finally {
      in.close
    }
  }

  def fromAssembly(files:Seq[File]):Program = {
    val in = new SequenceInputStream(files.map(new FileInputStream(_)).iterator)
    try {
      Assembler.assemble(in)
    } finally {
      in.close
    }
  }
}

case class Program(instructions:Array[Instruction], lineNumber:Array[Int])

object Assembler extends RegexParsers {
  val zero = 0 // zero register
  val at = 1   // temporaty regiser for psuedo instructions
  val sp = 29  // stack pointer
  val hp = 30  // heap pointer

  private var section = 0 // 0: text, 1: data
  private var pos = 0
  private val labels = mutable.Map[String, Int]()
  private var dataPos = 0
  private val dataLabels = mutable.Map[String, Int]()

  def decimal = "-?\\d+".r ^^ { BigInt(_) }
  def hex = "0x[0-9a-f]+".r ^^ { s => BigInt(s.substring(2), 16) }
  def int = hex | decimal
  def int32 = int >> { n =>
    if (Int.MinValue <= n && n <= Int.MaxValue)
      success(n.toInt)
    else
      failure("immediate value out of range: " + n.toString)
  }
  def int16 = int >> { n =>
    if (-(1 << 15) <= n && n < (1 << 15))
      success(n.toInt)
    else
      failure("immediate value out of range: " + n.toString)
  }
  def uint5 = int >> { n =>
    if (0 <= n && n < 32)
      success(n.toInt)
    else
      failure("immediate value out of range: " + n.toString)
  }
  def float = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { s =>
    Float.floatToRawIntBits(s.toFloat)
  }
  def label = "[\\w.]+".r ^? (labels orElse dataLabels, ("missing label: " + _))
  def paren[T](p:Parser[T]) = "(" ~> p <~ ")"
  def r = "r\\d+".r >> { s =>
    val n = BigInt(s.tail)
    if (n < 32) success(n.toInt) else failure("unknown register: " + s)
  }
  def r_ = r <~ ","
  def f = "f\\d+".r >> { s =>
    val n = BigInt(s.tail)
    if (n < 32) success(n.toInt) else failure("unknown register: " + s)
  }
  def f_ = f <~ ","

  val instTable:Map[String, Parser[Instruction]] = Map(
    // R format
    "sll"   -> (r_ ~ r_ ~ uint5 ^^ { case rd ~ rs ~ s => Sll(rd, rs, s) }),
    "srl"   -> (r_ ~ r_ ~ uint5 ^^ { case rd ~ rs ~ s => Srl(rd, rs, s) }),
    "sra"   -> (r_ ~ r_ ~ uint5 ^^ { case rd ~ rs ~ s => Sra(rd, rs, s) }),
    "sllv"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Sllv(rd, rs, rt) }),
    "srlv"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Srlv(rd, rs, rt) }),
    "srav"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Srav(rd, rs, rt) }),
    "jr"    -> (r ^^ { rs => Jr(rs) }),
    "jalr"  -> (r ^^ { rs => Jalr(rs) }),
    "mfhi"  -> (r ^^ { case rd => Mfhi(rd) }),
    "mthi"  -> (r ^^ { case rs => Mthi(rs) }),
    "mflo"  -> (r ^^ { case rd => Mflo(rd) }),
    "mtlo"  -> (r ^^ { case rs => Mtlo(rs) }),
    "mul"   -> (r_ ~ r ^^ { case rs ~ rt => Mul(rs, rt) }),
    "div"   -> (r_ ~ r ^^ { case rs ~ rt => Div(rs, rt) }),
    "add"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Add(rd, rs, rt) }),
    "sub"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Sub(rd, rs, rt) }),
    "and"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => And(rd, rs, rt) }),
    "or"    -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Or(rd, rs, rt) }),
    "xor"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Xor(rd, rs, rt) }),
    "nor"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Nor(rd, rs, rt) }),
    "slt"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Slt(rd, rs, rt) }),
    // I format
    "beq"   -> (r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Beq(rt, rs, a - pos - 1) }),
    "bne"   -> (r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Bne(rt, rs, a - pos - 1) }),
    "bltz"  -> (r_ ~ label ^^ { case rs ~ a => Bltz(rs, a - pos - 1) }),
    "bgez"  -> (r_ ~ label ^^ { case rs ~ a => Bgez(rs, a - pos - 1) }),
    "blez"  -> (r_ ~ label ^^ { case rs ~ a => Blez(rs, a - pos - 1) }),
    "bgtz"  -> (r_ ~ label ^^ { case rs ~ a => Bgtz(rs, a - pos - 1) }),
    "addi"  -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Addi(rt, rs, imm) }),
    "slti"  -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Slti(rt, rs, imm) }),
    "andi"  -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Andi(rt, rs, imm) }),
    "ori"   -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Ori(rt, rs, imm) }),
    "xori"  -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Xori(rt, rs, imm) }),
    "lui"   -> (r_ ~ int16 ^^ { case rt ~ imm => Lui(rt, imm) }),
    "bclf"  -> (r_ ~ int16 ^^ { case rt ~ imm => Bclf(rt, imm) }),
    "bclt"  -> (r_ ~ int16 ^^ { case rt ~ imm => Bclt(rt, imm) }),
    "imvf"  -> (f_ ~ r ^^ { case rt ~ rs => Imvf(rt, rs) }),
    "fmvi"  -> (r_ ~ f ^^ { case rt ~ rs => Fmvi(rt, rs) }),
    "lb"    -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lb(rt, rs, imm) }),
    "lh"    -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lh(rt, rs, imm) }),
    "lw"    -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lw(rt, rs, imm) }),
    "lbu"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lbu(rt, rs, imm) }),
    "lhu"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lhu(rt, rs, imm) }),
    "sb"    -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Sb(rt, rs, imm) }),
    "sh"    -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Sh(rt, rs, imm) }),
    "sw"    -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Sw(rt, rs, imm) }),
    "lwf"   -> (f_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lwf(rt, rs, imm) }),
    "swf"   -> (f_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Swf(rt, rs, imm) }),
    // J format
    "j"     -> (label ^^ { case a => J(a) }),
    "jal"   -> (label ^^ { case a => Jal(a) }),
    "halt"  -> success_ { Halt(pos) },
    // F format
    "fadd"  -> (f_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fadd(rd, rs, rt) }),
    "fsub"  -> (f_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fsub(rd, rs, rt) }),
    "fmul"  -> (f_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fmul(rd, rs, rt) }),
    "fdiv"  -> (f_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fdiv(rd, rs, rt) }),
    "fabs"  -> (f_ ~ f ^^ { case rd ~ rs => Fabs(rd, rs) }),
    "fneg"  -> (f_ ~ f ^^ { case rd ~ rs => Fneg(rd, rs) }),
    "finv"  -> (f_ ~ f ^^ { case rd ~ rs => Finv(rd, rs) }),
    "fsqrt" -> (f_ ~ f ^^ { case rd ~ rs => Fsqrt(rd, rs) }),
    "fcseq" -> (r_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fcseq(rd, rs, rt) }),
    "fclt"  -> (r_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fclt(rd, rs, rt) }),
    "fcle"  -> (r_ ~ f_ ~ f ^^ { case rd ~ rs ~ rt => Fcle(rd, rs, rt) }),
    // IO format
    "iw"    -> (r ^^ { case rd => Iw(rd) }),
    "ib"    -> (r ^^ { case rd => Ib(rd) }),
    "ih"    -> (r ^^ { case rd => Ih(rd) }),
    "ow"    -> (r ^^ { case rs => Ow(rs) }),
    "ob"    -> (r ^^ { case rs => Ob(rs) }),
    "oh"    -> (r ^^ { case rs => Oh(rs) }),
    "iwf"   -> (f ^^ { case rd => Iwf(rd) }),
    "owf"   -> (f ^^ { case rs => Owf(rs) }),
    "dump"  -> (r ^^ { case rs => Dump(rs) }),
    "dumpf" -> (f ^^ { case fs => Dumpf(fs) })
  )

  val pseudoInstTable:Map[String, (Int, Parser[List[Instruction]])] = Map(
    "move"  -> ((1, r_ ~ r ^^ { case rt ~ rs => List(Add(rt, rs, zero)) })),
    "neg"   -> ((1, r_ ~ r ^^ { case rt ~ rs => List(Sub(rt, zero, rs)) })),
    "lwx"   -> ((2, r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => List(Add(at, rs, rt), Lw(rd, at, 0)) })),
    "swx"   -> ((2, r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => List(Add(at, rs, rt), Sw(rd, at, 0)) })),
    "lwfx"  -> ((2, f_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => List(Add(at, rs, rt), Lwf(rd, at, 0)) })),
    "swfx"  -> ((2, f_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => List(Add(at, rs, rt), Swf(rd, at, 0)) })),
    "blt"   -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => List(Slt(at, rt, rs), Bgtz(at, a - pos - 2)) })),
    "ble"   -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => List(Sub(at, rt, rs), Blez(at, a - pos - 2)) })),
    "bgt"   -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => List(Slt(at, rs, rt), Bgtz(at, a - pos - 2)) })),
    "bge"   -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => List(Sub(at, rt, rs), Bgez(at, a - pos - 2)) })),
    "li"    -> ((1, r_ ~ int16 ^^ { case rt ~ imm => List(Ori(rt, zero, imm)) })),
    "la"    -> ((1, r_ ~ label ^^ { case rt ~ a => List(Lw(rt, zero, a)) })),
    "fmove" -> ((1, f_ ~ f ^^ { case rt ~ rs => List(Fadd(rt, rs, zero)) })),
    "fbeq"  -> ((2, f_ ~ f_ ~ label ^^ { case rt ~ rs ~ a => List(Fcseq(at, rt, rs), Bgtz(at, a - pos - 2)) })),
    "fbne"  -> ((2, f_ ~ f_ ~ label ^^ { case rt ~ rs ~ a => List(Fcseq(at, rt, rs), Blez(at, a - pos - 2)) })),
    "fblt"  -> ((2, f_ ~ f_ ~ label ^^ { case rt ~ rs ~ a => List(Fclt(at, rt, rs), Bgtz(at, a - pos - 2)) })),
    "fble"  -> ((2, f_ ~ f_ ~ label ^^ { case rt ~ rs ~ a => List(Fcle(at, rt, rs), Bgtz(at, a - pos - 2)) })),
    "fbgt"  -> ((2, f_ ~ f_ ~ label ^^ { case rt ~ rs ~ a => List(Fcle(at, rt, rs), Blez(at, a - pos - 2)) })),
    "fbge"  -> ((2, f_ ~ f_ ~ label ^^ { case rt ~ rs ~ a => List(Fclt(at, rt, rs), Blez(at, a - pos - 2)) })),
    "fli"   -> ((3, f_ ~ float ^^ { case rt ~ imm =>
                 List(Lui(at, imm >> 16), Ori(at, at, imm & 0xffff), Imvf(rt, at)) }))
  )

  val dirTable:Map[String, Parser[(List[Instruction], Int)]] = {
    def li(n:Int) =
      if (0 <= n && n < (1 << 16)) List(Ori(at, zero, n))
      else List(Lui(at, n >> 16), Ori(at, at, n & 0xffff))

    Map(
      "text"  -> success_ { section = 0; (Nil, 0) },
      "data"  -> success_ { section = 1; (Nil, 0) },
      "int"   -> (int32 ^^ { case v => (li(v) :+ Sw(at, zero, dataPos), 1) }),
      "float" -> (float ^^ { case v => (li(v) :+ Sw(at, zero, dataPos), 1) }),
      "addr"  -> (label ^^ { case a => (li(a) :+ Sw(at, zero, dataPos), 1) }),
      "space" -> (int16 ^^ { case s => (Nil, s) }),
      "fill"  -> (int16 ~ "," ~ int32 ^^ { case s ~ _ ~ v =>
                   (li(v) ++ List(Ori(2, zero, s), Addi(2, 2, -1), Sw(at, 2, dataPos), Bgtz(2, -3)), s) })
    )
  }

  def success_[T](v: => T):Parser[T] = commit(success(v))

  def doParse[T](p:Parser[T], s:String):T =
    parseAll(p, s) match {
      case Success(result, _) => result
      case e => sys.error(e.toString)
    }

  def reset() {
    section = 0
    pos = 0
    labels.clear()
    dataPos = 0
    dataLabels.clear()
  }

  def assemble(source:Source):Program = {
    reset()

    val labelPat = "\\s*([\\w.]+):\\s*".r
    val instPat  = "\\s*(\\w+)(.*)".r
    val dirPat   = "\\s*\\.(\\w+)(.*)".r
    val blankPat = "\\s*".r

    val main = new ArrayBuffer[(String, String, Int)]()
    val loader = new ArrayBuffer[Instruction]()

    loader += Xor(zero, zero, zero)
    loader += Imvf(zero, zero)
    loader += Ori(at, zero, 1)
    loader += Sll(hp, at, 10)
    loader += Sll(sp, at, 20)
    loader += Addi(sp, sp, -1)

    source.getLines.zipWithIndex.foreach { case (line, lineNr) =>
      line.replaceFirst("#.*$", "") match {
        case labelPat(label) =>
          if (section == 0) {
            labels(label) = pos
          } else {
            dataLabels(label) = dataPos
          }
        case instPat(opcode, operands) =>
          main += ((opcode, operands, lineNr + 1))
          pos += pseudoInstTable.get(opcode).map(_._1).getOrElse(1)
        case dirPat(directive, operands) =>
          if (dirTable.contains(directive)) {
            val (insts, size) = doParse(dirTable(directive), operands)
            loader ++= insts
            dataPos += size
          } else {
            sys.error("unknown directive: " + directive)
          }
        case blankPat() => // skip
        case _ => sys.error("invalid line: " ++ line)
      }
    }

    if (dataPos > (1 << 10)) {
      sys.error("too large data section")
    }

    val instructions = new Array[Instruction](loader.length + pos)
    val lineNumber = new Array[Int](loader.length + pos)

    loader.copyToArray(instructions)
    pos = loader.length
    labels.keys.foreach { label =>
      labels(label) += loader.length
    }

    main.iterator.foreach { case (opcode, operands, lineNr) =>
      if (instTable.contains(opcode)) {
        instructions(pos) = doParse(instTable(opcode), operands)
      } else if (pseudoInstTable.contains(opcode)) {
        doParse(pseudoInstTable(opcode)._2, operands).copyToArray(instructions, pos)
      } else {
        sys.error("unknown op: " + opcode)
      }

      val n = pseudoInstTable.get(opcode).map(_._1).getOrElse(1)
      (pos until (pos+n)).foreach(i => lineNumber(i) = lineNr)
      pos += n
    }

    Program(instructions, lineNumber)
  }

  def assemble(in:InputStream):Program = assemble(Source.fromInputStream(in))
}
