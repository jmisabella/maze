package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates, MazeRequest, Algorithm }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Sidewinder }

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid

  def generate(x: Int, y: Int): Grid = generate(Grid(x, y))

}

object Generator {
  def generate[A](request: MazeRequest)(implicit generator: A <:< Generator): Grid = {
    case object binaryTree extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object sidewinder extends Sidewinder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }

    request.algorithm match {
      case Algorithm.BinaryTree => binaryTree.generate(request.width, request.height)
      case Algorithm.Sidewinder => sidewinder.generate(request.width, request.height)
      case a => throw new IllegalArgumentException(s"Unexpected algorithm [$a]")
    }
  }

}
