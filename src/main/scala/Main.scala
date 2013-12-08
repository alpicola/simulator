package cpuex4

import java.io._

import scala.Console
import scala.collection.JavaConversions._

import Settings._

object Main {
  val usageText = "usage: simulator foo.s"

  def main(args:Array[String]) {
    val (settings, parsedArgs) = parseArgs(args.toList)

    if (parsedArgs.nonEmpty) {
      val program = Program.fromAssembly(parsedArgs.map(new File(_)))
      if (settings.assemble) {
        val dest = parsedArgs.last.replaceFirst("\\.[^\\.]*$", "")
        val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(dest)))
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
        case ("-a" | "--assemble")     :: rest => iter(settings.copy(assemble = true), rest)
        case ("-b" | "--binary-input") :: rest => iter(settings.copy(binMode = true), rest)
        case ("-p" | "--prof" | "-s")  :: rest => iter(settings.copy(keepStats = true), rest)
        case ("-f" | "--enable-fpu")   :: rest => iter(settings.copy(useFPU = true), rest)
        case "--dump-fops"             :: rest => iter(settings.copy(useFPU = true, dumpFops = true), rest)
        case "-o"              :: dest :: rest => iter(settings.copy(output = Some(dest)), rest)
        case arg :: _ if (arg.startsWith("-")) => sys.error("unknown option: " + arg)
        case _ => (settings, args)
      }
    }

    iter(defaultSettings, args)
  }
}
