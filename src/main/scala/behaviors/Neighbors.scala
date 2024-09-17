package maze.behaviors

import maze.classes.{ Coordinates, SquareNeighbors }
import maze.classes.MazeType._
import play.api.libs.json.{ Json, Format }

trait Neighbors{ 
  def toSeq(): Seq[Coordinates]
}
object Neighbors {
  def apply(mazeType: MazeType) = mazeType match {
    case Square => SquareNeighbors()
    case t => throw new IllegalArgumentException("Unexpected MazeType [" + mazeType + "]")
  }
}