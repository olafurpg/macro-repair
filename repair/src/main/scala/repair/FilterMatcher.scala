package repair

import scala.util.matching.Regex

class FilterMatcher(r: Regex) {
  def matches(id: String): Boolean = r.findFirstIn(id).nonEmpty
}
