package tel.schich.virtualecu

import tel.schich.virtualecu.FiniteDurationFunctions._
import tel.schich.virtualecu.Quantizers._
import scala.concurrent.duration.DurationDouble

class TestAction extends Action {

    //    def positiveSine(period: FiniteDuration): Double => Double = lag(period / 4) andThen sine(period) andThen shift(1) andThen scale(0.5)
    //    val coolantTemperature = add(linear(1, 8.minutes), gaussianRandom(-0.001, 0.001)) andThen lowCut(-40) andThen combined(90.0, scale, highCut) andThen shift(40)
    //    val engineRpm = add(product(positiveSine(5.minute), square(10.minute) andThen shift(1) andThen scale(0.5)) andThen scale(6000.0), positiveSine(10.minute)) andThen scale(6000.0)

    override def execute(t: Long): ActionResult = {
        val f = add(linear(1, 8.minutes), gaussianRandom(-0.001, 0.001)) andThen lowCut(-40) andThen combined(90.0, scale, highCut) andThen shift(40)
        val continuousResult = f(t)
        val result = unsignedInteger(1)(continuousResult)
        DataResponse(result)
    }
}
