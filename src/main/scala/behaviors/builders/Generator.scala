package maze.behaviors.builders

// import maze.classes.{ Cell, Grid, Coordinates, MazeRequest, Algorithm }
// import maze.behaviors.{ Linkage, Distance }
import maze.classes.{ Coordinates, MazeRequest, Algorithm }
import maze.behaviors.{ Linkage, Distance, ICell, IGrid, INeighbors }
import maze.behaviors.builders.{ BinaryTree, Sidewinder, AldousBroder, Wilsons, HuntAndKill }
import maze.classes.SquareNeighbors
import maze.classes.MazeType._
import scala.reflect.ClassTag

trait Generator[T <: MazeType, N <: INeighbors, C <: ICell[N], G <: IGrid[N, C]] {
  
  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE

  type DISTANCE <: Distance[N, C, G]
  val distance: DISTANCE
 
  def generate(grid: G): G

  def generate(mazeType: MazeType, width: Int, height: Int, start: Coordinates, goal: Coordinates)(implicit ct: ClassTag[C]): G = {
    generate(IGrid.instantiate[N, C, G](mazeType, height, width, start, goal))
  }
}

object Generator {
  def generate[T <: MazeType, A, N <: INeighbors, C <: ICell[N], G <: IGrid[N, C]](request: MazeRequest)(implicit generator: A <:< Generator[T, N, C, G], ct: ClassTag[C]): G = {
    case object binaryTree extends BinaryTree {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object sidewinder extends Sidewinder {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object aldousBroder extends AldousBroder {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object wilsons extends Wilsons {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object huntAndKill extends HuntAndKill {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object recursiveBacktracker extends RecursiveBacktracker {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    val generator: Generator[T, N, C, G] = (request.algorithm match {
      case Algorithm.BinaryTree => binaryTree
      case Algorithm.Sidewinder => sidewinder
      case Algorithm.AldousBroder => aldousBroder 
      case Algorithm.Wilsons => wilsons
      case Algorithm.HuntAndKill => huntAndKill 
      case Algorithm.RecursiveBacktracker => recursiveBacktracker
      case a => throw new IllegalArgumentException(s"Unexpected algorithm [$a]")
    }).asInstanceOf[Generator[T, N, C, G]]
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
