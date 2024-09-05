package maze.behaviors

import maze.classes.Coordinates
import play.api.libs.json.{ Json, Format }

trait INeighbors{ 
  def toSeq(): Seq[Coordinates]
}