package cpuex4

import Instruction._
import Program._

class Simulator(val program:Program) {
  val instructions = program.instructions

  // registers
  var pc = 0
  var hi = 0
  var lo = 0
  val r = new Array[Int](32)
  val f = new Array[Float](32)

  // register for return address
  val ra = 31

  // memory
  val ram = new Array[Int](1024 * 1024)

  def run() {
    pc = 0
    while (pc != instructions.length) {
      execute(instructions(pc))
    }
  }

  def execute(instruction:Instruction) {
    pc += 1
    instruction match {
      // R Format
      case Sll(rd, rs, shamt) => r(rd) = r(rs) << shamt
      case Srl(rd, rs, shamt) => r(rd) = r(rs) >>> shamt
      case Sra(rd, rs, shamt) => r(rd) = r(rs) >> shamt
      case Sllv(rd, rs, rt) => r(rd) = r(rs) << r(rt)
      case Srlv(rd, rs, rt) => r(rd) = r(rs) >>> r(rt)
      case Srav(rd, rs, rt) => r(rd) = r(rs) >> r(rt)
      case Jr(rs) => pc = r(rs)
      case Jalr(rs) => r(ra) = pc; pc = r(rs);
      case Mfhi(rd) => r(rd) = hi
      case Mthi(rs) => hi = r(rs)
      case Mflo(rd) => r(rd) = lo
      case Mtlo(rs) => lo = r(rs)
      case Mul(rs, rt) => val m = r(rs).toLong * r(rt).toLong
                          hi = (m >> 32).toInt; lo = (0xffff & m).toInt
      case Div(rs, rt) => hi = r(rs) / r(rt); lo = r(rs) % r(rt)
      case Add(rd, rs, rt) => r(rd) = r(rs) + r(rt)
      case Sub(rd, rs, rt) => r(rd) = r(rs) - r(rt)
      case And(rd, rs, rt) => r(rd) = r(rs) & r(rt)
      case Or(rd, rs, rt) =>  r(rd) = r(rs) | r(rt)
      case Xor(rd, rs, rt) => r(rd) = r(rs) ^ r(rt)
      case Nor(rd, rs, rt) => r(rd) = ~(r(rs) | r(rt))
      case Slt(rd, rs, rt) => r(rd) = if (r(rs) < r(rt)) 1 else 0
      // I Format
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
      // J Format
      case J(addr) => pc = addr
      case Jal(addr) => r(ra) = pc; pc = addr
      // IO Format
      case Ow(rs) => println(r(rs))
    }
  }

  def load(addr:Int):Int = {
    val i = addr >> 2
    val shift = (addr & 3) << 3
    if (shift == 0)
      ram(i)
    else
      ram(i) << shift | ram(i+1) >>> (32 - shift)
  }

  def store(addr:Int, n:Int) {
    val i = addr >> 2
    val shift = (addr & 3) << 3
    if (shift == 0) {
      ram(i) = n
    } else {
      ram(i) = (ram(i) >>> (32 - shift)) << (32 - shift) | n >>> shift
      ram(i+1) = (ram(i+1) << shift) >>> shift | n << (32 - shift)
    }
  }
}
