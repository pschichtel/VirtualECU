package tel.schich.virtualecu

import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.StandardOpenOption.{CREATE, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Paths}

import scala.concurrent.duration
import scala.concurrent.duration.{DurationLong, FiniteDuration}
import scala.math.Pi
import scala.util.Try

object TesterMain {

    def parseDuration(d: String): Try[FiniteDuration] = {
        val pattern = "^((?:[1-9]\\d*|0))\\s+(\\S+).*$".r

        Try {
            d.trim match {
                case pattern(lengthStr, unitStr) =>
                    val length = lengthStr.toLong
                    val unit = unitStr.toLowerCase match {
                        case "ns" => duration.NANOSECONDS
                        case "nano" => duration.NANOSECONDS
                        case "nanos" => duration.NANOSECONDS
                        case "nanosecond" => duration.NANOSECONDS
                        case "nanoseconds" => duration.NANOSECONDS
                        case "µs" => duration.MICROSECONDS
                        case "micro" => duration.MICROSECONDS
                        case "micros" => duration.MICROSECONDS
                        case "microsecond" => duration.MICROSECONDS
                        case "microseconds" => duration.MICROSECONDS
                        case "ms" => duration.MILLISECONDS
                        case "millis" => duration.MILLISECONDS
                        case "millisecond" => duration.MILLISECONDS
                        case "milliseconds" => duration.MILLISECONDS
                        case "s" => duration.SECONDS
                        case "sec" => duration.SECONDS
                        case "secs" => duration.SECONDS
                        case "second" => duration.SECONDS
                        case "seconds" => duration.SECONDS
                        case "min" => duration.MINUTES
                        case "minute" => duration.MINUTES
                        case "minutes" => duration.MINUTES
                        case "h" => duration.HOURS
                        case "hour" => duration.HOURS
                        case "hours" => duration.HOURS
                        case "d" => duration.DAYS
                        case "day" => duration.DAYS
                        case "days" => duration.DAYS
                        case s => throw new IllegalArgumentException(s"Unknown unit: $s")
                    }

                    new FiniteDuration(length, unit)
                case s => throw new IllegalArgumentException(s"Incompatible duration format: $s")
            }
        }
    }

    val f: Double => Double = {
        import FiniteDurationFunctions._

        //chooseWeighted(1.0 -> 0.0, 20.0 -> 2.0, 10.0 -> 3.0, 100000.0 -> 4.0)
//        accumulate(0, chooseWeighted(1.0 -> -1.0, 50.0 -> 0.0, 1.0 -> 1.0)) andThen clamp(0, 6)
//        accumulate(0, choose(-1.0, 0.0, 1.0)) andThen clamp(0, 6)
//        chooseWeighted(1.0 -> -1.0, 2.0 -> 0.0, 1.0 -> 1.0)
//        multiply(sum(saw(30.seconds) andThen shift(1) andThen scale(0.5), sine(3000.millis) andThen shift(1) andThen scale(0.5), sine(1500.millis) andThen shift(1) andThen scale(0.5)) andThen scale(2500), selectPartialConstant(randomWalk(0, 0, 6, 1.0 -> -1.0, 1000.0 -> 0.0, 1.0 -> 1.0), 0, 1.0/4.31, 1.0/2.44, 1.0/1.35, 1.0/0.94, 1.0/0.82, 1.0/0.7)) andThen scale(1.0/(Pi * 0.508 * 3.6 * 3.5))
        saw(5.minutes) andThen negate andThen shift(1) andThen scale(0.5)
    }

    def main(args: Array[String]): Unit = {
        if (args.length != 2) {
            println("Usage: <duration> <output file>")
            System.exit(1)
        }

        val duration = parseDuration(args(0)).get
        val outputPath = Paths.get(args(1))

        val writer = Files.newBufferedWriter(outputPath, US_ASCII, CREATE, TRUNCATE_EXISTING, WRITE)
        for (t <- 0L until duration.toMillis) {
            val v = f(t)
            writer.write(s"$t,$v\n")
        }
        writer.close()
    }
}
