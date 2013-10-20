package cpuex4

import java.io._
import java.util._

import scala.collection.mutable

import Instruction._
import Program._

class Simulator(val program:Program, val settings:Settings) {
  import program.{instructions, labels}
  import settings.{keepStats, binMode}

  // registers
  private[this] var pc = 0
  private[this] var hi = 0
  private[this] var lo = 0
  private[this] val r = new Array[Int](32)
  private[this] val f = new Array[Float](32)

  // register for return address
  private[this] val ra = 31

  // memory
  private[this] val ram = new Array[Int](1024 * 1024)

  private[this] val execStats = new Array[Int](instructions.length)
  private[this] val callStats = new Array[Int](instructions.length)

  private[this] val scanner = new Scanner(System.in)
  private[this] val binIn = new DataInputStream(System.in)
  private[this] val binOut = new DataOutputStream(System.out)

  private[this] var elapsed = 0L

  def reset() {
    pc = 0
    hi = 0
    lo = 0
    (0 to 31).foreach { i =>
      r(i) = 0
      f(i) = 0
    }
    (0 until instructions.length).foreach { i =>
      execStats(i) = 0
      callStats(i) = 0
    }
  }

  def run() {
    reset()

    val instructions = program.instructions

    if (keepStats) {
      val start = System.currentTimeMillis()
      while (pc != instructions.length) {
        execStats(pc) += 1
        execute(instructions(pc))
      }
      elapsed = System.currentTimeMillis() - start
      reportStats()
    } else {
      while (pc != instructions.length) {
        execute(instructions(pc))
      }
    }
  }

  def execute(instruction:Instruction) {
    pc += 1
    instruction match {
      // R format
      case Sll(rd, rs, shamt) => r(rd) = r(rs) << shamt
      case Srl(rd, rs, shamt) => r(rd) = r(rs) >>> shamt
      case Sra(rd, rs, shamt) => r(rd) = r(rs) >> shamt
      case Sllv(rd, rs, rt) => r(rd) = r(rs) << r(rt)
      case Srlv(rd, rs, rt) => r(rd) = r(rs) >>> r(rt)
      case Srav(rd, rs, rt) => r(rd) = r(rs) >> r(rt)
      case Jr(rs) => pc = r(rs)
      case Jalr(rs) => r(ra) = pc; pc = r(rs); if (keepStats) callStats(pc) += 1;
      case Mfhi(rd) => r(rd) = hi
      case Mthi(rs) => hi = r(rs)
      case Mflo(rd) => r(rd) = lo
      case Mtlo(rs) => lo = r(rs)
      case Mul(rs, rt) => val m = r(rs).toLong * r(rt); hi = (m >> 32).toInt; lo = (0xffff & m).toInt
      case Div(rs, rt) => hi = r(rs) / r(rt); lo = r(rs) % r(rt)
      case Add(rd, rs, rt) => r(rd) = r(rs) + r(rt)
      case Sub(rd, rs, rt) => r(rd) = r(rs) - r(rt)
      case And(rd, rs, rt) => r(rd) = r(rs) & r(rt)
      case Or(rd, rs, rt) =>  r(rd) = r(rs) | r(rt)
      case Xor(rd, rs, rt) => r(rd) = r(rs) ^ r(rt)
      case Nor(rd, rs, rt) => r(rd) = ~(r(rs) | r(rt))
      case Slt(rd, rs, rt) => r(rd) = if (r(rs) < r(rt)) 1 else 0
      // I format
      case Beq(rt, rs, imm) => if (r(rt) == r(rs)) pc += imm
      case Bne(rt, rs, imm) => if (r(rt) != r(rs)) pc += imm
      case Bltz(rs, imm) => if (r(rs) < 0) pc += imm
      case Bgez(rs, imm) => if (r(rs) >= 0) pc += imm
      case Blez(rs, imm) => if (r(rs) <= 0) pc += imm
      case Bgtz(rs, imm) => if (r(rs) > 0) pc += imm
      case Addi(rt, rs, imm) => r(rt) = r(rs) + imm
      case Slti(rt, rs, imm) => r(rt) = if (r(rs) < imm) 1 else 0
      case Andi(rt, rs, imm) => r(rt) = r(rs) & imm
      case Ori(rt, rs, imm) =>  r(rt) = r(rs) | imm
      case Xori(rt, rs, imm) => r(rt) = r(rs) ^ imm
      case Lui(rt, imm) => r(rt) = imm << 16
      case Bclf(rt, imm) => if (r(rt) == 0) pc += imm
      case Bclt(rt, imm) => if (r(rt) == 1) pc += imm
      case Lw(rt, rs, imm) => r(rt) = load(r(rs) + imm)
      case Sw(rt, rs, imm) => store(r(rs) + imm, r(rt))
      // J format
      case J(addr) => pc = addr
      case Jal(addr) => r(ra) = pc; pc = addr; if (keepStats) callStats(pc) += 1;
      // IO format
      case Iw(rs) => r(rs) = if (binMode) binIn.readInt() else scanner.nextInt()
      case Ib(rs) => r(rs) = (if (binMode) binIn.readByte() else scanner.nextByte()).toInt
      case Ih(rs) => r(rs) = (if (binMode) binIn.readShort() else scanner.nextShort()).toInt
      case Ow(rs) => if (binMode) binOut.writeInt(r(rs)) else println(r(rs))
      case Ob(rs) => if (binMode) binOut.writeByte(r(rs).toByte) else println(r(rs).toByte)
      case Oh(rs) => if (binMode) binOut.writeShort(r(rs).toShort) else println(r(rs).toShort)
      
      case _ => sys.error("not yet implemented op: " + instruction.getName)
    }
  }

  def load(addr:Int):Int = {
    val i = addr >> 2
    val j = addr & 3
    if (j == 0) {
      ram(i)
    } else {
      val shift = j << 3
      val shift_ = 32 - j
      ram(i) << shift | ram(i+1) >>> shift_
    }
  }

  def store(addr:Int, n:Int) {
    val i = addr >> 2
    val j = addr & 3
    if (j == 0) {
      ram(i) = n
    } else {
      val shift = j << 3
      val shift_ = 32 - j
      ram(i) = (ram(i) >>> shift_) << shift_ | n >>> shift
      ram(i+1) = (ram(i+1) << shift) >>> shift | n << shift_
    }
  }

  def reportStats() {
    import scala.Console.err

    val instStats = mutable.Map[String, Int]()

    (0 until instructions.length).foreach { i =>
      val op = instructions(i).getName
      instStats(op) = instStats.getOrElse(op, 0) + execStats(i) 
    }
    
    val decending = Ordering[Int].reverse
    val table1 = instStats.toSeq.sortBy(_._2)(decending)
    val table2 = labels.mapValues(i => callStats(i)).toSeq.sortBy(_._2)(decending)
    val issued = table1.iterator.map(_._2).foldLeft(0L)(_ + _)

    err.println("1. General")
    err.println("total time\t" + elapsed.toString + " ms")
    err.println("total issued\t" + issued.toString + " insts")
    
    err.println("2. Issue")
    table1.foreach { case (op, count) =>
      err.println(op + "\t" + count.toString)
    }

    err.println("3. Call")
    table2.iterator.takeWhile(_._2 > 0).foreach { case (label, count) =>
      err.println(label + "\t" + count.toString)
    }
  }
}
