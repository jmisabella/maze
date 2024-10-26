package maze.behaviors.builders

import maze.classes.{ Coordinates, MazeRequest, Algorithm }
import maze.behaviors.{ Linkage, Distance, Cell, Grid }
import maze.behaviors.builders.{ BinaryTree, Sidewinder, AldousBroder, Wilsons, HuntAndKill }
import maze.classes.cell.{ SquareCell, TriangleCell, HexCell }
import maze.classes.grid.{ SquareGrid, TriangleGrid, HexGrid }
import maze.classes.MazeType._
import scala.reflect.ClassTag

trait Generator[C <: Cell, G <: Grid[C]] {

  type LINKAGE <: Linkage[C, G]
  val linker: LINKAGE

  type DISTANCE <: Distance[C, G]
  val distance: DISTANCE
 
  def generate(grid: G)(implicit ct: ClassTag[C]): G

  def generate(mazeType: MazeType, width: Int, height: Int, start: Coordinates, goal: Coordinates)(implicit ct: ClassTag[C]): G = {
    generate(Grid.instantiate[C, G](mazeType, height, width, start, goal))
  }

  def generate(request: MazeRequest)(implicit ct: ClassTag[C]): G = {
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
      case Orthogonal => gen[SquareCell, SquareGrid](request)
      case Delta => gen[TriangleCell, TriangleGrid](request)
      case Sigma => gen[HexCell, HexGrid](request)
    }
  }
  // def generate(request: MazeRequest): Grid[_] = {
  def gen[C <: Cell, G <: Grid[C]](request: MazeRequest): G = {
    case object squareBinaryTree extends BinaryTree[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    case object squareSidewinder extends Sidewinder[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    case object squareAldousBroder extends AldousBroder[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    case object triangleAldousBroder extends AldousBroder[TriangleCell, TriangleGrid] {
      case object _linkage extends Linkage[TriangleCell, TriangleGrid]
      override type LINKAGE = Linkage[TriangleCell, TriangleGrid]
      override val linker = _linkage
      case object _distance extends Distance[TriangleCell, TriangleGrid]
      override type DISTANCE = Distance[TriangleCell, TriangleGrid]
      override val distance = _distance
    }
    case object hexAldousBroder extends AldousBroder[HexCell, HexGrid] {
      case object _linkage extends Linkage[HexCell, HexGrid]
      override type LINKAGE = Linkage[HexCell, HexGrid]
      override val linker = _linkage
      case object _distance extends Distance[HexCell, HexGrid]
      override type DISTANCE = Distance[HexCell, HexGrid]
      override val distance = _distance
    }
    case object squareWilsons extends Wilsons[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    case object triangleWilsons extends Wilsons[TriangleCell, TriangleGrid] {
      case object _linkage extends Linkage[TriangleCell, TriangleGrid]
      override type LINKAGE = Linkage[TriangleCell, TriangleGrid]
      override val linker = _linkage
      case object _distance extends Distance[TriangleCell, TriangleGrid]
      override type DISTANCE = Distance[TriangleCell, TriangleGrid]
      override val distance = _distance
    }
    case object hexWilsons extends Wilsons[HexCell, HexGrid] {
      case object _linkage extends Linkage[HexCell, HexGrid]
      override type LINKAGE = Linkage[HexCell, HexGrid]
      override val linker = _linkage
      case object _distance extends Distance[HexCell, HexGrid]
      override type DISTANCE = Distance[HexCell, HexGrid]
      override val distance = _distance
    }
    case object squareHuntAndKill extends HuntAndKill[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    case object triangleHuntAndKill extends HuntAndKill[TriangleCell, TriangleGrid] {
      case object _linkage extends Linkage[TriangleCell, TriangleGrid]
      override type LINKAGE = Linkage[TriangleCell, TriangleGrid]
      override val linker = _linkage
      case object _distance extends Distance[TriangleCell, TriangleGrid]
      override type DISTANCE = Distance[TriangleCell, TriangleGrid]
      override val distance = _distance
    }
    case object hexHuntAndKill extends HuntAndKill[HexCell, HexGrid] {
      case object _linkage extends Linkage[HexCell, HexGrid]
      override type LINKAGE = Linkage[HexCell, HexGrid]
      override val linker = _linkage
      case object _distance extends Distance[HexCell, HexGrid]
      override type DISTANCE = Distance[HexCell, HexGrid]
      override val distance = _distance
    }
    case object squareRecursiveBacktracker extends RecursiveBacktracker[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    case object triangleRecursiveBacktracker extends RecursiveBacktracker[TriangleCell, TriangleGrid] {
      case object _linkage extends Linkage[TriangleCell, TriangleGrid]
      override type LINKAGE = Linkage[TriangleCell, TriangleGrid]
      override val linker = _linkage
      case object _distance extends Distance[TriangleCell, TriangleGrid]
      override type DISTANCE = Distance[TriangleCell, TriangleGrid]
      override val distance = _distance
    }
    case object hexRecursiveBacktracker extends RecursiveBacktracker[HexCell, HexGrid] {
      case object _linkage extends Linkage[HexCell, HexGrid]
      override type LINKAGE = Linkage[HexCell, HexGrid]
      override val linker = _linkage
      case object _distance extends Distance[HexCell, HexGrid]
      override type DISTANCE = Distance[HexCell, HexGrid]
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
    generator.generate(request).asInstanceOf[G]
  }

}
