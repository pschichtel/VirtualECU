package tel.schich.virtualecu

import tel.schich.virtualecu.FiniteDurationFunctions._
import tel.schich.virtualecu.Quantizers._
import scala.concurrent.duration.DurationDouble

class TestAction extends Action {
    override def execute(t: Long): ActionResult = {
        val f = add(linear(1, 8.minutes), gaussianRandom(-0.001, 0.001)) andThen lowCut(-40) andThen combined(90.0, scale, highCut) andThen shift(40)
        val continuousResult = f(t.toDouble)
        val result = unsignedInteger(1)(continuousResult)
        DataResponse(result)
    }
}
