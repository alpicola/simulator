package cpuex4

import java.io._

import scala.Console

import Instruction._
import Program._
import Settings._

object Main {
  val usageText = "usage: simulator foo.s"

  def main(args:Array[String]) {
    val (settings, parsedArgs) = parseArgs(args.toList)

    if (parsedArgs.nonEmpty) {
      val program = Program.fromAssembly(parsedArgs)
      if (settings.assemble) {
        val dest = parsedArgs.last.replaceFirst("\\.[^\\.]*$", "")
        val out = new DataOutputStream(new FileOutputStream(dest))
        try {
          program.instructions.foreach { instruction =>
            out.writeInt(instruction.toInt)
          }
        } finally {
          out.close
        }
      } else {
        settings.output.foreach { dest =>
          Console.setOut(new FileOutputStream(dest))
        }
        val simulator = new Simulator(program, settings)
        simulator.run
      }
    } else {
      println(usageText)
    }
  }

  def parseArgs(args:List[String]):(Settings, List[String]) = {
    def iter(settings:Settings, args:List[String]):(Settings, List[String]) = {
      args match {
        case "-a" :: rest => iter(settings.copy(assemble = true), rest)
        case "-b" :: rest => iter(settings.copy(binMode = true), rest)
        case "-s" :: rest => iter(settings.copy(keepStats = true), rest)
        case "-o" :: dest :: rest => iter(settings.copy(output = Some(dest)), rest)
        case _ => (settings, args)
      }
    }

    iter(defaultSettings, args)
  }
}
