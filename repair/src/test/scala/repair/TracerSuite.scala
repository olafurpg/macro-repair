package repair

import org.junit.Test
import org.junit.Assert
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TracerSuite {
  // Statically guarantee a and b have same type.
  def assertEquals[T](a: T)(b: T): Unit = Assert.assertEquals(a, b)

  @Test
  def basic(): Unit = {
    val a = 2
    val b = 3
    val obtained = Tracer.trace(b == a)
    assertEquals(obtained)(
      List(
        TestValue("b", "Int", 3),
        TestValue("a", "Int", 2)
      ))
  }
}
