package maze.behaviors.builders

import maze.classes.{ Coordinates, MazeRequest, Algorithm }
import maze.behaviors.{ Linkage, Distance, Cell, Grid, Neighbors }
import maze.behaviors.builders.{ BinaryTree, Sidewinder, AldousBroder, Wilsons, HuntAndKill }
import maze.classes.{ SquareNeighbors, SquareCell, SquareGrid }
import maze.classes.MazeType._
import scala.reflect.ClassTag

trait Generator[N <: Neighbors, C <: Cell, G <: Grid[C]] {

  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE

  type DISTANCE <: Distance[N, C, G]
  val distance: DISTANCE
 
  def generate(grid: G)(implicit ct: ClassTag[C]): G

  def generate(mazeType: MazeType, width: Int, height: Int, start: Coordinates, goal: Coordinates)(implicit ct: ClassTag[C]): G = {
    generate(Grid.instantiate[N, C, G](mazeType, height, width, start, goal))
  }
}

object Generator {
  def generate(request: MazeRequest): Grid[_] = {
    val generator = (request.mazeType, request.algorithm) match {

      case (Square, Algorithm.BinaryTree) => {
        // BinaryTree algorithm only eligible for Square MazeType
        case object binaryTree extends BinaryTree[SquareNeighbors, SquareCell, SquareGrid] {
          case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override val linker = _linkage
          case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
          override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
          override val distance = _distance
        }
        binaryTree
      }
      case (Square, Algorithm.Sidewinder) => {
        // Sidewinder algorithm only eligible for Square MazeType
        case object sidewinder extends Sidewinder[SquareNeighbors, SquareCell, SquareGrid] {
          case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override val linker = _linkage
          case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
          override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
          override val distance = _distance
        }
        sidewinder
      }
      case (Square, Algorithm.AldousBroder) => {
        case object aldousBroder extends AldousBroder[SquareNeighbors, SquareCell, SquareGrid] {
          case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override val linker = _linkage
          case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
          override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
          override val distance = _distance
        }
        aldousBroder
      }
      case (Square, Algorithm.Wilsons) => {
        case object wilsons extends Wilsons[SquareNeighbors, SquareCell, SquareGrid] {
          case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override val linker = _linkage
          case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
          override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
          override val distance = _distance
        }
        wilsons
      }
      case (Square, Algorithm.HuntAndKill) => {
        case object huntAndKill extends HuntAndKill[SquareNeighbors, SquareCell, SquareGrid] {
          case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override val linker = _linkage
          case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
          override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
          override val distance = _distance
        }
        huntAndKill
      }
      case (Square, Algorithm.RecursiveBacktracker) => {
        case object recursiveBacktracker extends RecursiveBacktracker[SquareNeighbors, SquareCell, SquareGrid] {
          case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
          override val linker = _linkage
          case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
          override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
          override val distance = _distance
        }
        recursiveBacktracker
      }
      case (t, Algorithm.BinaryTree) => throw new IllegalArgumentException(s"BinaryTree algorithm only eligible for Square maze type. It is not eligible for [$t]")
      case (t, Algorithm.Sidewinder) => throw new IllegalArgumentException(s"Sidewinder algorithm only eligible for Square maze type. It is not eligible for [$t]")
      case (t, _) => throw new IllegalArgumentException(s"Unexpected maze type [$t]")
      case (_, a) => throw new IllegalArgumentException(s"Unexpected algorithm [$a]")
    }
    // generator.distance.pathTo( 
      val distances = generator.distance.distances(
        generator.generate(request.mazeType, request.width, request.height, request.start, request.goal)
        , request.start.x
        , request.start.y)
      println("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH\n" + distances.toString()) 
      // , request.start.x
      // , request.start.y 
      // , request.goal.x
      // , request.goal.y)
    generator.distance.pathTo( 
      generator.distance.distances(
        generator.generate(request.mazeType, request.width, request.height, request.start, request.goal)
        , request.start.x
        , request.start.y)
      , request.start.x
      , request.start.y 
      , request.goal.x
      , request.goal.y)
  }

}
