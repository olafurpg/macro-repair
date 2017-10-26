package repair

import org.junit.Test
import org.junit.Assert
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TracerSuite {
  case class A(a: Int)
  def assertEquals[T](expected: T)(obtained: T): Unit =
    Assert.assertEquals(expected, obtained)
  @Test
  def unapply(): Unit = {
    val a = 2
    val b = 3
    val obtained = Tracer.trace({
      b == a
    })
    val expected = List(
      TestValue("b", "Int", 3),
      TestValue("a", "Int", 2)
    )
    assertEquals(expected)(obtained)
  }
}
