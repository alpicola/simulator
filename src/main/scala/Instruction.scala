package cpuex4

object Instruction {
  type Reg = Int

  trait R extends Instruction {
    val op = 0
    val rs:Reg
    val rt:Reg
    val rd:Reg
    val shamt:Int
    val funct:Int
    def toInt = op << 26 | rs << 21 | rt << 16 | rd << 11 | shamt << 6 | funct
  }

  trait I extends Instruction {
    val rs:Reg
    val rt:Reg
    val imm:Int
    def toInt = op << 26 | rs << 21 | rt << 16 | imm & 0xffff
  }

  trait J_ extends Instruction {
    val addr:Int
    def toInt = op << 26 | addr
  }

  trait F extends R {
    override val op = 17
    val shamt = 0
  }

  trait IO extends R {
    override val op = 30
    val shamt = 0
  }

  // R format

  case class Sll(rd:Reg, rs:Reg, shamt:Int) extends Instruction with R { val rt = 0; val funct = 0 }
  case class Srl(rd:Reg, rs:Reg, shamt:Int) extends Instruction with R { val rt = 0; val funct = 2 }
  case class Sra(rd:Reg, rs:Reg, shamt:Int) extends Instruction with R { val rt = 0; val funct = 3 }
  case class Sllv(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val shamt = 0; val funct = 4 }
  case class Srlv(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val shamt = 0; val funct = 6 }
  case class Srav(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val shamt = 0; val funct = 7 }
  case class Jr(rs:Reg) extends Instruction with R   { val rt = 0; val rd = 0; val shamt = 0; val funct = 8 }
  case class Jalr(rs:Reg) extends Instruction with R { val rt = 0; val rd = 0; val shamt = 0; val funct = 9 }
  case class Mfhi(rd:Reg) extends Instruction with R { val rs = 0; val rt = 0; val shamt = 0; val funct = 16 }
  case class Mthi(rs:Reg) extends Instruction with R { val rt = 0; val rd = 0; val shamt = 0; val funct = 17 }
  case class Mflo(rd:Reg) extends Instruction with R { val rs = 0; val rt = 0; val shamt = 0; val funct = 18 }
  case class Mtlo(rs:Reg) extends Instruction with R { val rt = 0; val rd = 0; val shamt = 0; val funct = 19 }
  case class Mul(rs:Reg, rt:Reg) extends Instruction with R  { val rd = 0; val shamt = 0; val funct = 24 }
  case class Mulu(rs:Reg, rt:Reg) extends Instruction with R { val rd = 0; val shamt = 0; val funct = 25 }
  case class Div(rs:Reg, rt:Reg) extends Instruction with R  { val rd = 0; val shamt = 0; val funct = 26 }
  case class Divu(rs:Reg, rt:Reg) extends Instruction with R { val rd = 0; val shamt = 0; val funct = 27 }
  case class Add(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val shamt = 0; val funct = 32 }
  case class Addu(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val shamt = 0; val funct = 33 }
  case class Sub(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val shamt = 0; val funct = 34 }
  case class Subu(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val shamt = 0; val funct = 35 }
  case class And(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val shamt = 0; val funct = 36 }
  case class Or(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R   { val shamt = 0; val funct = 37 }
  case class Xor(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val shamt = 0; val funct = 38 }
  case class Nor(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val shamt = 0; val funct = 39 }
  case class Slt(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val shamt = 0; val funct = 42 }
  case class Sltu(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val shamt = 0; val funct = 43 }

  // I format

  case class Beq(rt:Reg, rs:Reg, imm:Int) extends Instruction with I { val op = 4 }
  case class Bne(rt:Reg, rs:Reg, imm:Int) extends Instruction with I { val op = 5 }
  case class Bltz(rs:Reg, imm:Int) extends Instruction with I { val op = 6; val rt = 0 }
  case class Bgez(rs:Reg, imm:Int) extends Instruction with I { val op = 6; val rt = 1 }
  case class Blez(rs:Reg, imm:Int) extends Instruction with I { val op = 7; val rt = 0 }
  case class Bgtz(rs:Reg, imm:Int) extends Instruction with I { val op = 7; val rt = 1 }
  case class Addi(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 8 }
  case class Addiu(rt:Reg, rs:Reg, imm:Int) extends Instruction with I { val op = 9 }
  case class Slti(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 10 }
  case class Sltiu(rt:Reg, rs:Reg, imm:Int) extends Instruction with I { val op = 11 }
  case class Andi(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 12 }
  case class Ori(rt:Reg, rs:Reg, imm:Int) extends Instruction with I   { val op = 13 }
  case class Xori(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 14 }
  case class Lui(rt:Reg, imm:Int) extends Instruction with I  { val op = 15; val rs = 0 }
  case class Bclf(rt:Reg, imm:Int) extends Instruction with I { val op = 16; val rs = 0 }
  case class Bclt(rt:Reg, imm:Int) extends Instruction with I { val op = 16; val rs = 1 }
  case class Lb(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 32 } 
  case class Lh(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 33 }
  case class Lw(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 35 }
  case class Lbu(rt:Reg, rs:Reg, imm:Int) extends Instruction with I { val op = 36 } 
  case class Lhu(rt:Reg, rs:Reg, imm:Int) extends Instruction with I { val op = 37 } 
  case class Sb(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 40 }
  case class Sh(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 41 }
  case class Sw(rt:Reg, rs:Reg, imm:Int) extends Instruction with I  { val op = 43 }

  // J format

  case class J(addr:Int) extends Instruction with J_    { val op = 2 }
  case class Jal(addr:Int) extends Instruction with J_  { val op = 3 }
  case class Halt(addr:Int) extends Instruction with J_ { val op = 2 }

  // F format

  // case class Fadd(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val funct = 0 }
  // case class Fsub(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val funct = 1 }
  // case class Fmul(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val funct = 2 }
  // case class Fdiv(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val funct = 3 }
  // case class Fabs(rd:Reg, rs:Reg) extends Instruction with R  { val rt = 0; val funct = 5 }
  // case class Fneg(rd:Reg, rs:Reg) extends Instruction with R  { val rt = 0; val funct = 7 }
  // case class Finv(rd:Reg, rs:Reg) extends Instruction with R  { val rt = 0; val funct = 8 }
  // case class Fsqrt(rd:Reg, rs:Reg) extends Instruction with R { val rt = 0; val funct = 9 }
  // case class Fcseq(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R { val funct = 50 }
  // case class Fclt(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val funct = 52 }
  // case class Fcle(rd:Reg, rs:Reg, rt:Reg) extends Instruction with R  { val funct = 54 }

  // IO format

  case class Iw(rd:Reg) extends Instruction with IO { val rs = 0; val rt = 0; val funct = 3 }
  case class Ib(rd:Reg) extends Instruction with IO { val rs = 0; val rt = 0; val funct = 4 }
  case class Ih(rd:Reg) extends Instruction with IO { val rs = 0; val rt = 0; val funct = 6 }
  case class Ow(rs:Reg) extends Instruction with IO { val rt = 0; val rd = 0; val funct = 11 }
  case class Ob(rs:Reg) extends Instruction with IO { val rt = 0; val rd = 0; val funct = 12 }
  case class Oh(rs:Reg) extends Instruction with IO { val rt = 0; val rd = 0; val funct = 13 }
  // case class Iwf(rd:Reg) extends Instruction with IO { val rs = 0; val rt = 0; val funct = 17 } 
  // case class Owf(rs:Reg) extends Instruction with IO { val rt = 0; val rd = 0; val funct = 25 }
}

sealed abstract class Instruction {
  val op:Int
  def toInt:Int
  def getName:String = {
    val className = getClass.getName
    val i = className.lastIndexOf("$")
    className.substring(i+1).toLowerCase
  }
}
