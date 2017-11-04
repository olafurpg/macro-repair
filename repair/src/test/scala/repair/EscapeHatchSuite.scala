package repair

import org.junit.Test
import org.junit.Assert
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class EscapeHatchSuite {
  @Test
  def f(): Unit = {
    val obtained = F.f { Option("lol" * 3).get + "hello" }
    val expected = Some(("lol" * 3) + "hello")
    Assert.assertEquals(expected, obtained)
  }
//  @Test
//  def escapeHatch(): Unit = {
//    import EscapeHatch._
//    val comments = List(
//      Comment(1, "a", Off),
//      Comment(3, "b", Off),
//      Comment(4, "b", On),
//      Comment(4, "a", On)
//    )
//    val hatch = EscapeHatch(comments)
//    assert(hatch.isEnabled("a", -1))
//    assert(hatch.isEnabled("b", -1))
//    assert(hatch.isDisabled("b", 3))
//    assert(hatch.isDisabled("b", 4))
//    assert(hatch.isDisabled("a", 1))
//    assert(hatch.isDisabled("a", 3))
//    assert(hatch.isEnabled("a", 5))
//  }

}

//@RunWith(classOf[JUnit4])
//class TracerSuite {
//  // Statically guarantee a and b have same type.
//  def assertEquals[T](a: T)(b: T): Unit = Assert.assertEquals(a, b)
//
//  @Test
//  def basic(): Unit = {
//    val a = 2
//    val b = 3
//    val obtained = Tracer.trace(b == a)
//    assertEquals(obtained)(
//      List(
//        TestValue("b", "Int", 3),
//        TestValue("a", "Int", 2)
//      ))
//  }
//}
