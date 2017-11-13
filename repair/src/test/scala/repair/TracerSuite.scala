package repair

import org.junit.Test
import org.junit.Assert
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TracerSuite {
  @Test
  def basic(): Unit = {
    type MyString = String
    val a: Array[String] = Array("a")
    val b = 4
    val obtained = Tracer.trace(
      b.toString == a.head
    )
    pprint.log(obtained)
  }

}
