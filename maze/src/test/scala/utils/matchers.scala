package utils

import com.maze.game.Items.Item
import com.maze.game.MovementResults.{MovementResult, NewCell}
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.Set

/**
  * Created by kgusakov on 10.08.16.
  */
trait CustomMatchers {

  class MoveResultItemsMatcher(item: Item) extends Matcher[MovementResult] {

    override def apply(left: MovementResult) = {
      val matchResult = left match {
        case NewCell(_, items) if items == Set(item) => true
        case _ => false
      }
      MatchResult(
        matchResult,
        s"""It's not cell with only one item ${item.toString}""",
        s"""Cell has only item $item"""
      )
    }
  }

  def hasOnlyItem(item: Item) = new MoveResultItemsMatcher(item)
}
