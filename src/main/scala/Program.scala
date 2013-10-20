package cpuex4

import java.io._
import scala.io.Source
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

import Instruction._

object Program {
  def fromAssembly(file:String):Program = {
    val in = new FileInputStream(file)
    try {
      AssemblyParser.parse(in)
    } finally {
      in.close
    }
  }

  def fromAssembly(files:Seq[String]):Program = {
    val in = new SequenceInputStream(files.iterator.map(new FileInputStream(_)))
    try {
      AssemblyParser.parse(in)
    } finally {
      in.close
    }
  }
}

case class Program(instructions:Array[Instruction], labels:Map[String, Int])

object AssemblyParser extends RegexParsers {
  private var pos = 0
  private val labels = mutable.Map[String, Int]()

  def int32 = "-?\\d+".r >> { s =>
    val n = BigInt(s)
    if (Int.MinValue <= n && n <= Int.MaxValue)
      success(n.toInt)
    else
      failure("immediate value out of range: " + n.toString)
  }
  def int16 = "-?\\d+".r >> { s =>
    val n = BigInt(s)
    if (-(1 << 15) <= n && n < (1 << 15))
      success(n.toInt)
    else
      failure("immediate value out of range: " + n.toString)
  }
  def uint5 = "\\d+".r >> { s =>
    val n = BigInt(s)
    if (n < 32)
      success(n.toInt)
    else
      failure("immediate value out of range: " + n.toString)
  }
  def label = "[\\w.]+".r ^? (labels, ("missing label: " + _))
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
    "sll"  -> (r_ ~ r_ ~ uint5 ^^ { case rd ~ rs ~ s => Sll(rd, rs, s) }),
    "srl"  -> (r_ ~ r_ ~ uint5 ^^ { case rd ~ rs ~ s => Srl(rd, rs, s) }),
    "sra"  -> (r_ ~ r_ ~ uint5 ^^ { case rd ~ rs ~ s => Sra(rd, rs, s) }),
    "sllv" -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Sllv(rd, rs, rt) }),
    "srlv" -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Srlv(rd, rs, rt) }),
    "srav" -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Srav(rd, rs, rt) }),
    "jr"   -> (r ^^ { rs => Jr(rs) }),
    "jalr" -> (r ^^ { rs => Jalr(rs) }),
    "mul"  -> (r_ ~ r ^^ { case rs ~ rt => Mul(rs, rt) }),
    "div"  -> (r_ ~ r ^^ { case rs ~ rt => Div(rs, rt) }),
    "add"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Add(rd, rs, rt) }),
    "sub"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Sub(rd, rs, rt) }),
    "and"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => And(rd, rs, rt) }),
    "or"   -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Or(rd, rs, rt) }),
    "xor"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Xor(rd, rs, rt) }),
    "nor"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Nor(rd, rs, rt) }),
    "slt"  -> (r_ ~ r_ ~ r ^^ { case rd ~ rs ~ rt => Slt(rd, rs, rt) }),
    // I format
    "beq"  -> (r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Beq(rt, rs, a - pos - 1) }),
    "bne"  -> (r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Bne(rt, rs, a - pos - 1) }),
    "bltz" -> (r_ ~ label ^^ { case rs ~ a => Bltz(rs, a - pos - 1) }),
    "bgez" -> (r_ ~ label ^^ { case rs ~ a => Bgez(rs, a - pos - 1) }),
    "blez" -> (r_ ~ label ^^ { case rs ~ a => Blez(rs, a - pos - 1) }),
    "bgtz" -> (r_ ~ label ^^ { case rs ~ a => Bgtz(rs, a - pos - 1) }),
    "addi" -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Addi(rt, rs, imm) }),
    "slti" -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Slti(rt, rs, imm) }),
    "andi" -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Andi(rt, rs, imm) }),
    "ori"  -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Ori(rt, rs, imm) }),
    "xori" -> (r_ ~ r_ ~ int16 ^^ { case rt ~ rs ~ imm => Xori(rt, rs, imm) }),
    "lui"  -> (r_ ~ int16 ^^ { case rt ~ imm => Lui(rt, imm) }),
    "bclf" -> (r_ ~ int16 ^^ { case rt ~ imm => Bclf(rt, imm) }),
    "bclt" -> (r_ ~ int16 ^^ { case rt ~ imm => Bclt(rt, imm) }),
    "lb"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lb(rt, rs, imm) }),
    "lh"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lh(rt, rs, imm) }),
    "lw"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lw(rt, rs, imm) }),
    "lbu"  -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lbu(rt, rs, imm) }),
    "lhu"  -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Lhu(rt, rs, imm) }),
    "sb"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Sb(rt, rs, imm) }),
    "sh"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Sh(rt, rs, imm) }),
    "sw"   -> (r_ ~ int16 ~ paren(r) ^^ { case rt ~ imm ~ rs => Sw(rt, rs, imm) }),
    // J format
    "j"    -> (label ^^ { case a => J(a) }),
    "jal"  -> (label ^^ { case a => Jal(a) }),
    "halt" -> success(Halt(pos)),
    // IO format
    "iw"   -> (r ^^ { case rd => Iw(rd) }),
    "ib"   -> (r ^^ { case rd => Ib(rd) }),
    "ih"   -> (r ^^ { case rd => Ih(rd) }),
    "ow"   -> (r ^^ { case rs => Ow(rs) }),
    "ob"   -> (r ^^ { case rs => Ob(rs) }),
    "oh"   -> (r ^^ { case rs => Oh(rs) })
  )

  val zero = 0 // zero register
  val at = 1   // temporaty regiser for psuedo instructions
  val pseudoInstTable:Map[String, (Int, Parser[Array[Instruction]])] = Map(
    "li"   -> ((2, r_ ~ int32 ^^ { case rt ~ imm => Array(Lui(rt, imm >> 16), Ori(rt, rt, imm & 0xffff)) })),
    "lli"  -> ((1, r_ ~ int16 ^^ { case rt ~ imm => Array(Ori(rt, zero, imm)) })),
    "move" -> ((1, r_ ~ r ^^ { case rt ~ rs => Array(Add(rt, rs, zero)) })),
    "blt"  -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Array(Slt(at, rt, rs), Bgtz(at, a - pos - 2)) })),
    "ble"  -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Array(Sub(at, rt, rs), Blez(at, a - pos - 2)) })),
    "bgt"  -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Array(Slt(at, rs, rt), Bgtz(at, a - pos - 2)) })),
    "bge"  -> ((2, r_ ~ r_ ~ label ^^ { case rt ~ rs ~ a => Array(Sub(at, rt, rs), Bgez(at, a - pos - 2)) }))
  )

  def parse(source:Source):Program = {
    val lines = new ArrayBuffer[(Int, String, String)]()
    val labelPat = "\\s*([\\w.]+):\\s*".r
    val instPat  = "\\s*(\\w+)(.*)".r
    pos = 0
    labels.clear()

    source.getLines.foreach { line =>
      line.replaceFirst("#.*$", "") match {
        case labelPat(label) =>
          labels(label) = pos
        case instPat(opcode, operands) =>
          lines += ((pos, opcode, operands))
          pos += pseudoInstTable.get(opcode).map(_._1).getOrElse(1)
        case _ => // skip
      }
    }

    val instructions = new Array[Instruction](pos)

    lines.foreach { case (p, opcode, operands) =>
      pos = p
      if (instTable.contains(opcode)) {
        parseAll(instTable(opcode), operands) match {
          case Success(result, _) => instructions(pos) = result
          case e => sys.error(e.toString)
        }
      } else if (pseudoInstTable.contains(opcode)) {
        parseAll(pseudoInstTable(opcode)._2, operands) match {
          case Success(result, _) => result.copyToArray(instructions, pos)
          case e => sys.error(e.toString)
        }
      } else {
        sys.error("unknown op: " + opcode)
      }
    }

    Program(instructions, labels.toMap)
  }

  def parse(in:InputStream):Program = parse(Source.fromInputStream(in))
}
