package tel.schich.virtualecu

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import net.jcazevedo.moultingyaml._

import scala.concurrent.duration.{DurationDouble, FiniteDuration}
import scala.io.Source

object Main {

    val Name = "Virtual ECU"

    def main(args: Array[String]): Unit = {

        args match {
            case Array(interfaceName, path) =>
                val p = Paths.get(path)
                if (!Files.exists(p) || !Files.isReadable(p)) {
                    println("File not found or not readable!")
                } else {
                    startEmulation(interfaceName, p)
                }
            case _ =>
                println("Usage: <can interface> <config path>")
        }

    }

    def startEmulation(interfaceName: String, conf: Path): Unit = {
        import EmulationProtocol._
        val emulation = Source.fromFile(conf.toFile, "UTF-8").mkString.parseYaml.convertTo[Emulation]
        println(s"$interfaceName $emulation")

        import Functions._

        val timeFrame = 15.minutes

//        val f = addAll(lag(5.seconds) andThen saw(10.seconds), sin(5.seconds), lag(1.second) andThen sin(7.seconds), cos(2.seconds)) andThen scale(10)
        val tri = triangle(5.minutes) andThen shift(1) andThen scale(0.5)
        val f = product(linear(1, 8.minutes) andThen combined(89.9, scale, highCut), tri, square(1.minute))

        val writer = Files.newBufferedWriter(Paths.get("output.csv"), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
        for (x <- 0L to timeFrame.toMillis) {
            writer.write(s"$x,${f(x)}\n")
        }
        writer.close()
    }

}
