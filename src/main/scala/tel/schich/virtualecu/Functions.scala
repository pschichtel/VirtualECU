package tel.schich.virtualecu

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object PreFunctions {
}

trait Domain[T] {
    def toDouble(v: T): Double
}

object FiniteDurationFunctions extends Functions[FiniteDuration] {
    override def toDouble(v: FiniteDuration): Double = v.toMillis
}

case class Rate[U](unit: Double, per: U)

abstract class Functions[U] {
    def toDouble(v: U): Double

    private val rnd = new Random()

    type F = Double => Double

    def lag(offset: Double): F = _ - offset
    def lag(offset: U): F = lag(toDouble(offset))

    def advance(offset: Double): F = _ + offset
    def advance(offset: U): F = advance(toDouble(offset))

    def round: F = _.round
    def floor: F = _.floor
    def ceiling: F = _.ceil

    def scale(s: Double): F = _ * s

    def shift(n: Double): F = _ + n

    def lowCut(n: Double): F = v => if (v < n) n else v
    def lowPass(n: Double): F = v => if (v < n) v else n
    def highCut(n: Double): F = v => if (v > n) n else v
    def highPass(n: Double): F = v => if (v > n) v else n

    def invert: F = 1/_

    def negate: F = -_

    def signum: F = _.signum

    def saw(period: U): F = {
        val p = toDouble(period)

        x => 2 * ((x / p) - math.floor(0.5 + (x / p)))
    }

    def constant[T](c: T): Double => T = _ => c

    def linear(period: U): F = linear(1, period)

    def linear(rate: Rate[U]): F = linear(rate.unit, rate.per)

    def linear(unit: Double, period: U): F = {
        val m = unit / toDouble(period)
        t => t * m
    }

    def log: F = math.log10

    def ln: F = math.log

    def log(base: Double): F = math.log(_) / math.log(base)

    def exp: F = math.exp

    def exp(base: Double): F = math.pow(base, _)

    def square(period: U): F = sine(period) andThen signum

    def triangle(period: U): F = {
        val p = toDouble(period)

        t => 2 * math.abs(2 * ((t / p) - math.floor((t / p) + 0.5))) - 1
    }

    def boxcar(at: U, holdingFor: U): F = {
        val start = toDouble(at)
        val end = start + toDouble(holdingFor)
        t => {
            if (t >= start && t < end) 1.0
            else 0
        }
    }

    def power(n: Double): F = math.pow(_, n)

    def derive(delta: U)(f: F): F = derive(toDouble(delta))(f)
    def derive(delta: Double)(f: F): F = t => f(t) - f(t - delta)

    def integrate(delta: U)(f: F): F = integrate(toDouble(delta))(f)
    def integrate(delta: Double)(f: F): F = t => (f(t - delta) + f(t)) / 2.0

    def absolute: F = math.abs

    def sine(period: U): F = trig(math.sin, period)

    def cosine(period: U): F = trig(math.cos, period)

    def tangens(period: U): F = trig(math.tan, period)

    def arcusTangens(period: U): F = trig(math.atan, period)

    def trig(f: Double => Double, period: U): F = {
        val p = toDouble(period) / (2 * math.Pi)
        t => f(t / p)
    }

    def sigmoid: F = t => 1.0 / (1 + math.exp(-t))

    def add(a: F, b: F): F = {
        t => a(t) + b(t)
    }

    def sum(f: F*): F = f.reduce(add)

    def multiply(a: F, b: F): F = {
        t => a(t) * b(t)
    }

    def product(f: F*): F = f.reduce(multiply)

    def combine[T](a: T => F, b: T => F): T => F = {
        in => a(in) andThen b(in)
    }

    def combineAll[T](f: T => F*): T => F = f.reduce(combine[T])

    def combined[T](v: T, f: T => F*): F = combineAll(f: _*)(v)

    def gaussianRandom: F =
        _ => rnd.nextGaussian()

    def gaussianRandom(from: Double, to: Double): F =
        _ => from + rnd.nextGaussian() * (to - from)

    def random: F =
        _ => math.random() * 2 - 1

    def random(from: Double, to: Double): F =
        _ => from + (rnd.nextDouble() * (to - from))

}

object Quantizers {

    def bigInt(v: Double): BigInt = BigDecimal(v).toBigInt()

    def unsignedInteger(size: Int)(v: Double): Array[Byte] =
        bigDecimalToArray(BigDecimal(v).max(0), size)

    def integer(size: Int)(v: Double): Array[Byte] =
        bigDecimalToArray(BigDecimal(v), size)

    private def bigDecimalToArray(i: BigDecimal, size: Int): Array[Byte] = {
        val allBytes = i.toBigInt().toByteArray

        if (size == allBytes.length) allBytes
        else allBytes.slice(allBytes.length - size, allBytes.length)
    }

    def getBytes(charset: String): String => Array[Byte] = _.getBytes(charset)
}
