package cpuex4

import Instruction._
import Program._

object Main {
  def main(args:Array[String]) = {
    if (args.nonEmpty) {
      val program = Program.fromAssembly(args)
      println(program.instructions.toList)
    } else {
      println("usage: simulator foo.s")
    }
  }
}
