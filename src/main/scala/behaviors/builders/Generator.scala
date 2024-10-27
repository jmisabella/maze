package maze.behaviors.builders

import maze.classes.{ Coordinates, MazeRequest, Algorithm, Cell, Grid }
// import maze.behaviors.{ Linkage, Distance, Cell, Grid }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Sidewinder, AldousBroder, Wilsons, HuntAndKill }
// import maze.classes.cell.{ SquareCell, TriangleCell, HexCell }
// import maze.classes.grid.{ SquareGrid, TriangleGrid, HexGrid }
import maze.classes.MazeType._
import scala.reflect.ClassTag

trait Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid

  def generate(mazeType: MazeType, width: Int, height: Int, start: Coordinates, goal: Coordinates): Grid = {
    generate(Grid(mazeType, height, width, start, goal))
  }

  def generate(request: MazeRequest): Grid = {
    distance.pathTo(
      distance.distances(
        generate(request.mazeType, request.width, request.height, request.start, request.goal)
        , request.start.x
        , request.start.y)
      , request.start.x
      , request.start.y 
      , request.goal.x
      , request.goal.y)
  }

}

object Generator {
  def generate(request: MazeRequest)  = {
    request.mazeType match {
      case Orthogonal => gen(request)
      case Delta => gen(request)
      case Sigma => gen(request)
    }
  }
  // def generate(request: MazeRequest): Grid[_] = {
  def gen(request: MazeRequest): Grid = {
    case object squareBinaryTree extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object squareSidewinder extends Sidewinder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object squareAldousBroder extends AldousBroder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object triangleAldousBroder extends AldousBroder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object hexAldousBroder extends AldousBroder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object squareWilsons extends Wilsons {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object triangleWilsons extends Wilsons {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object hexWilsons extends Wilsons {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object squareHuntAndKill extends HuntAndKill {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object triangleHuntAndKill extends HuntAndKill {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object hexHuntAndKill extends HuntAndKill {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object squareRecursiveBacktracker extends RecursiveBacktracker {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object triangleRecursiveBacktracker extends RecursiveBacktracker {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object hexRecursiveBacktracker extends RecursiveBacktracker {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    val generator = (request.mazeType, request.algorithm) match  {
      case (Orthogonal, Algorithm.BinaryTree)  => squareBinaryTree
      case (Orthogonal, Algorithm.Sidewinder)  => squareSidewinder
      case (Orthogonal, Algorithm.AldousBroder)  => squareAldousBroder
      // case (Delta, Algorithm.AldousBroder) => triangleAldousBroder
      // case (Sigma, Algorithm.AldousBroder) => hexAldousBroder
      case (Orthogonal, Algorithm.Wilsons)  => squareWilsons
      // case (Delta, Algorithm.Wilsons) => triangleWilsons
      // case (Sigma, Algorithm.Wilsons) => hexWilsons
      case (Orthogonal, Algorithm.HuntAndKill)  => squareHuntAndKill
      // case (Delta, Algorithm.HuntAndKill) => triangleHuntAndKill
      // case (Sigma, Algorithm.HuntAndKill) => hexHuntAndKill
      case (Orthogonal, Algorithm.RecursiveBacktracker)  => squareRecursiveBacktracker
      // case (Delta, Algorithm.RecursiveBacktracker) => triangleRecursiveBacktracker
      // case (Sigma, Algorithm.RecursiveBacktracker) => hexRecursiveBacktracker
      case (t, Algorithm.BinaryTree) => throw new IllegalArgumentException(s"BinaryTree algorithm only eligible for Square maze type. It is not eligible for [$t]")
      case (t, Algorithm.Sidewinder) => throw new IllegalArgumentException(s"Sidewinder algorithm only eligible for Square maze type. It is not eligible for [$t]")
      case (t, a) => throw new IllegalAccessError(s"Rejected MazeType,Algorithm combination [$t,$a]")
    }
    generator.generate(request)
  }

}
