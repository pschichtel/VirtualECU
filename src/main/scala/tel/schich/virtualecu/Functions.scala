package tel.schich.virtualecu

import scala.concurrent.duration.FiniteDuration

object PreFunctions {
}

case class Rate(unit: Double, per: FiniteDuration)
object Functions {

    type F = Double => Double

    def lag(offset: Double): F = _ - offset

    def lag(offset: FiniteDuration): F = lag(offset)

    def advance(offset: Double): F = _ + offset

    def advance(offset: FiniteDuration): F = advance(offset)

    def round: F = _.round

    def scale(s: Double): F = _ * s

    def shift(n: Double): F = _ + n

    def lowCut(n: Double): F = v => if (v < n) n else v
    def lowPass(n: Double): F = v => if (v < n) v else n
    def highCut(n: Double): F = v => if (v > n) n else v
    def highPass(n: Double): F = v => if (v > n) v else n

    def invert: F = -_

    def signum: F = _.signum

    def saw(period: FiniteDuration): F = {
        val p = period.toMillis

        x => 2 * ((x / p) - math.floor(0.5 + (x / p)))
    }

    def constant(n: Double): F = _ => n

    def linear(period: FiniteDuration): F = linear(1, period)

    def linear(rate: Rate): F = linear(rate.unit, rate.per)

    def linear(unit: Double, period: FiniteDuration): F = {
        val m = unit/period.toMillis
        t => t * m
    }

    def log: F = math.log10

    def ln: F = math.log

    def log(base: Double): F = math.log(_) / math.log(base)

    def exp: F = math.exp

    def exp(base: Double): F = math.pow(base, _)

    def square(period: FiniteDuration): F = sine(period) andThen signum

    def triangle(period: FiniteDuration): F = {
        val p = period.toMillis

        t => 2 * math.abs(2 * ((t / p) - math.floor((t / p) + 0.5))) - 1
    }

    def boxcar(at: FiniteDuration, holding: FiniteDuration): F = {
        val start = at.toMillis.toDouble
        val end = start + holding.toMillis
        t => {
            if (t >= start && t < end) 1.0
            else 0
        }
    }

    def power(n: Double): F = math.pow(_, n)

    def derive(delta: FiniteDuration)(f: F): F = derive(delta.toMillis)(f)
    def derive(delta: Double)(f: F): F = t => f(t) - f(t - delta)

    def integrate(delta: FiniteDuration)(f: F): F = integrate(delta.toMillis)(f)
    def integrate(delta: Double)(f: F): F = t => (f(t - delta) + f(t)) / 2.0

    def absolute: F = math.abs

    def sine(period: FiniteDuration): F = trig(math.sin, period)

    def cosine(period: FiniteDuration): F = trig(math.cos, period)

    def tangens(period: FiniteDuration): F = trig(math.tan, period)

    def trig(f: Double => Double, period: FiniteDuration): F = {
        val p = period.toMillis.toDouble / (2 * math.Pi)
        t => f(t / p)
    }

    def add(a: F, b: F): F = {
        t => a(t) + b(t)
    }

    def sum(f: F*): F = {
        t => f.reduce(add)(t)
    }

    def multiply(a: F, b: F): F = {
        t => a(t) * b(t)
    }

    def product(f: F*): F = {
        t => f.reduce(multiply)(t)
    }

    def combine[T](a: T => F, b: T => F): T => F = {
        in => a(in) andThen b(in)
    }

    def combineAll[T](f: T => F*): T => F = {
        f.reduce(combine[T])
    }

    def combined[T](v: T, f: T => F*): F = {
        t => combineAll(f: _*)(v)(t)
    }

}
