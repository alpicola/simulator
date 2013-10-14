package cpuex4

import Instruction._
import Program._

object Main {
  def main(args:Array[String]) {
    if (args.nonEmpty) {
      val program = Program.fromAssembly(args)
      val simulator = new Simulator(program)
      simulator.run
    } else {
      println("usage: simulator foo.s")
    }
  }
}
