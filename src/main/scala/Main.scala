package cpuex4

import Instruction._
import AssemblyParser._

object Main {
  def main(args:Array[String]) = {
    if (args.nonEmpty) {
      val instructions = AssemblyParser.parseFromFile(args(0))
      instructions.foreach(println)
    } else {
      println("usage: simulator foo.s")
    }
  }
}
