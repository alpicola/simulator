package cpuex4

import java.io._
import java.util._
import java.lang.Float

import scala.collection.mutable
import scala.math._
import scala.Console

import Instruction._

class Simulator(val program:Program, val settings:Settings) {
  import program.{instructions, lineNumber}
  import settings.{keepStats, binMode, useFPU, dumpFops}

  // registers
  private[this] var pc = 0
  private[this] var hi = 0
  private[this] var lo = 0
  private[this] val r = new Array[Int](32)
  private[this] val f = new Array[Float](32)

  // memory
  private[this] val ram = new Array[Int](1 << 20)

  private[this] val ra = 31 // return address

  private[this] val scanner = new Scanner(System.in)
  private[this] val binIn = new DataInputStream(System.in)
  private[this] val buf = new Array[Byte](4)

  private[this] val fpu = new FPU(dumpFops)

  def reset() {
    pc = 0
  }

  def run() {
    reset()

    val instructions = program.instructions
    var _pc = 0 // previous pc for debug

    try {
      if (keepStats) {
        val start = System.currentTimeMillis()
        val stats = new Array[Int](instructions.length)
        while (pc != instructions.length) {
          stats(pc) += 1
          execute(instructions(pc))
          _pc = pc
        }
        reportStats(System.currentTimeMillis() - start, stats)
      } else {
        while (pc != instructions.length) {
          execute(instructions(pc))
        }
      }
    } catch {
      case e:ArrayIndexOutOfBoundsException =>
        if (0 <= pc && pc < instructions.length) {
          val addr = instructions(_pc) match {
            case Lw(_, rs, imm) => r(rs) + imm
            case Sw(_, rs, imm) => r(rs) + imm
            case Lwf(_, rs, imm) => r(rs) + imm
            case Swf(_, rs, imm) => r(rs) + imm
            case _ => throw e
          }
          sys.error("invalid memory access: " + addr.toString + 
                    " (at line: " + lineNumber(_pc).toString + ")")
        } else {
          sys.error("invalid jump: " + pc.toString +
                    " (at line: " + lineNumber(_pc).toString + ")")
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
      case Jalr(rs) => r(ra) = pc; pc = r(rs)
      case Mfhi(rd) => r(rd) = hi
      case Mthi(rs) => hi = r(rs)
      case Mflo(rd) => r(rd) = lo
      case Mtlo(rs) => lo = r(rs)
      case Mul(rs, rt) => val m = r(rs).toLong * r(rt); hi = (m >> 32).toInt; lo = (0xffffffff & m).toInt
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
      case Imvf(ft, rs) => f(ft) = Float.intBitsToFloat(r(rs))
      case Fmvi(rt, fs) => r(rt) = Float.floatToRawIntBits(f(fs))
      case Lw(rt, rs, imm) => r(rt) = ram(r(rs) + imm)
      case Sw(rt, rs, imm) => ram(r(rs) + imm) = r(rt)
      case Lwf(ft, rs, imm) => f(ft) = Float.intBitsToFloat(ram(r(rs) + imm))
      case Swf(ft, rs, imm) => ram(r(rs) + imm) = Float.floatToRawIntBits(f(ft))
      // J format
      case J(addr) => pc = addr
      case Jal(addr) => r(ra) = pc; pc = addr
      case Halt(_) => pc = instructions.length
      // F format
      case Fadd(fd, fs, ft) => f(fd) = if (useFPU) fpu.fadd(f(fs), f(ft)) else f(fs) + f(ft)
      case Fsub(fd, fs, ft) => f(fd) = if (useFPU) fpu.fsub(f(fs), f(ft)) else f(fs) - f(ft)
      case Fmul(fd, fs, ft) => f(fd) = if (useFPU) fpu.fmul(f(fs), f(ft)) else f(fs) * f(ft)
      case Fdiv(fd, fs, ft) => f(fd) = if (useFPU) fpu.fdiv(f(fs), f(ft)) else f(fs) / f(ft)
      case Fabs(fd, fs) => f(fd) = abs(f(fs))
      case Fneg(fd, fs) => f(fd) = -f(fs)
      case Finv(fd, fs) => f(fd) = if (useFPU) fpu.finv(f(fs)) else 1.0f / f(fs)
      case Fsqrt(fd, fs) => f(fd) = if (useFPU) fpu.fsqrt(f(fs)) else sqrt(f(fs).toDouble).toFloat
      case Fcseq(rd, fs, ft) => r(rd) = if (f(fs) == f(ft)) 1 else 0
      case Fclt(rd, fs, ft) => r(rd) = if (f(fs) < f(ft)) 1 else 0
      case Fcle(rd, fs, ft) => r(rd) = if (f(fs) <= f(ft)) 1 else 0
      // IO format
      case Iw(rd) => r(rd) = if (binMode) binIn.readInt() else scanner.nextInt()
      case Ib(rd) => r(rd) = (if (binMode) binIn.readByte() else scanner.nextByte()).toInt
      case Ih(rd) => r(rd) = (if (binMode) binIn.readShort() else scanner.nextShort()).toInt
      // case Ow(rs) =>
      case Ob(rs) => buf(0) = r(rs).toByte; Console.out.write(buf, 0, 1)
      // case Oh(rs) =>
      case Iwf(fd) => f(fd) = if (binMode) binIn.readFloat() else scanner.nextFloat()
      // case Owf(fs) =>
      case Dump(rs) => Console.err.println(r(rs))
      case Dumpf(fs) => Console.err.println(f(fs))
      
      case _ => sys.error("not yet implemented op: " + instruction.getName)
    }
  }

  def reportStats(elapsed:Long, execStats:Array[Int]) {
    val instStats = mutable.Map[String, Int]()

    (0 until instructions.length).foreach { i =>
      val op = instructions(i).getName
      instStats(op) = instStats.getOrElse(op, 0) + execStats(i) 
    }
    
    val decending = Ordering[Int].reverse
    val table = instStats.toSeq.sortBy(_._2)(decending)
    val issued = table.iterator.map(_._2).foldLeft(0L)(_ + _)

    Console.err.println("total time\t" + elapsed.toString + " ms")
    Console.err.println("total issued\t" + issued.toString + " insts")
    
    table.takeWhile(t => t._2 > 0).foreach { case (op, count) =>
      Console.err.println(op + "\t" + count.toString)
    }
  }
}
