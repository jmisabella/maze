package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates, MazeRequest, Algorithm, MazeType }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Sidewinder }

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid

  // def generate(x: Int, y: Int): Grid = generate(Grid(x, y))
  //// x indicates horizontal (number of columns) while y indicates vertical (number of rows)
  def generate(x: Int, y: Int): Grid = generate(Grid(y, x))

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
    val generator = request.algorithm match {
      case Algorithm.BinaryTree => binaryTree
      case Algorithm.Sidewinder => sidewinder
      case a => throw new IllegalArgumentException(s"Unexpected algorithm [$a]")
    }
    request.mazeType match {
      case MazeType.Unsolved => generator.generate(request.width, request.height)
      case MazeType.DistanceMap => {
        generator.distance.distances(
          generator.generate(request.width, request.height)
          , 0
          , 0)
      }
      case MazeType.Solved => {
        generator.distance.pathTo( 
          generator.distance.distances(
            generator.generate(request.width, request.height)
            , 0
            , request.width - 1)
          , 0
          , request.width - 1 
          , request.height - 1
          , 0)
      }
      // case MazeType.Solved => {
      //   generator.distance.pathTo( 
      //     generator.distance.distances(
      //       generator.generate(request.width, request.height)
      //       , 0
      //       , 0)
      //     , 0
      //     , 0
      //     , request.height - 1
      //     , request.width - 1)
      // }
    } 
  }

}
