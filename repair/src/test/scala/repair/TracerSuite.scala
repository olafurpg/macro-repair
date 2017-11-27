package repair

import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TracerSuite {
  @Test
  def basic(): Unit = {
    val b = "banana"
    val obtained = Main.identity(b)
    Assert.assertEquals("b", obtained)
  }

}
