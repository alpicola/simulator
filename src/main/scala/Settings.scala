package cpuex4

object Settings {
  def defaultSettings = Settings(false, false, false, None)
}

case class Settings(val assemble:Boolean, val keepStats:Boolean,
                    val binMode:Boolean, val output:Option[String])
