package repair

import scala.collection.immutable.TreeMap

class EscapeHatch(
    starts: TreeMap[Int, FilterMatcher],
    ends: TreeMap[Int, FilterMatcher]) {
  def isEnabled(id: String, pos: Int): Boolean = !isDisabled(id, pos)
  def isDisabled(id: String, pos: Int): Boolean = {
    starts.to(pos).exists {
      case (spos, sname) =>
        sname.matches(id) && {
          !ends.range(spos, pos + 1).exists {
            case (epos, ename) =>
              ename.matches(id)
          }
        }

    }
  }
}

object EscapeHatch {
  case class Comment(pos: Int, id: String, kind: Kind)
  sealed abstract class Kind
  case object On extends Kind
  case object Off extends Kind
  def apply(comments: List[Comment]): EscapeHatch = {
    val starts = TreeMap.newBuilder[Int, FilterMatcher]
    val ends = TreeMap.newBuilder[Int, FilterMatcher]
    comments.foreach {
      case Comment(pos, id, Off) => starts += (pos -> new FilterMatcher(id.r))
      case Comment(pos, id, On) => ends += (pos -> new FilterMatcher(id.r))
    }
    new EscapeHatch(starts.result(), ends.result())
  }
}