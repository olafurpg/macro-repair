package repair

import org.junit.Test
import org.junit.Assert
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TracerSuite {
  @Test
  def basic(): Unit = {
    val obtained = Tracer.trace {
      val List(x) = List(2)
      x == 2
    }
    pprint.log(obtained)
  }

}
