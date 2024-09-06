package maze.behaviors.builders

// import maze.classes.{ Cell, Grid, Coordinates, MazeRequest, Algorithm }
// import maze.behaviors.{ Linkage, Distance }
import maze.classes.{ Coordinates, MazeRequest, Algorithm }
import maze.behaviors.{ Linkage, Distance, ICell, IGrid, INeighbors }
import maze.behaviors.builders.{ BinaryTree, Sidewinder, AldousBroder, Wilsons, HuntAndKill }
import maze.classes.{ SquareNeighbors, SquareCell, SquareGrid }
import maze.classes.MazeType._
import scala.reflect.ClassTag

trait Generator[N <: INeighbors, C <: ICell, G <: IGrid[C]] {

  // type MAZE_TYPE <: MazeType
  // val mazeType: MAZE_TYPE

  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE

  type DISTANCE <: Distance[N, C, G]
  val distance: DISTANCE
 
  def generate(grid: G)(implicit ct: ClassTag[C]): G

  def generate(mazeType: MazeType, width: Int, height: Int, start: Coordinates, goal: Coordinates)(implicit ct: ClassTag[C]): G = {
    generate(IGrid.instantiate[N, C, G](mazeType, height, width, start, goal))
  }
}

object Generator {
  def generate[A, N <: INeighbors, C <: ICell, G <: IGrid[C]](request: MazeRequest)(implicit generator: A <:< Generator[N, C, G], ct: ClassTag[C]): G = {
    // case object binaryTree extends BinaryTree {
    //   case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
    //   override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
    //   override val linker = _linkage
    //   case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
    //   override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
    //   override val distance = _distance
    // }
    // case object sidewinder extends Sidewinder {
    //   case object _linkage extends Linkage[SquareNeighbors, SquareCell, SquareGrid]
    //   override type LINKAGE = Linkage[SquareNeighbors, SquareCell, SquareGrid]
    //   override val linker = _linkage
    //   case object _distance extends Distance[SquareNeighbors, SquareCell, SquareGrid]
    //   override type DISTANCE = Distance[SquareNeighbors, SquareCell, SquareGrid]
    //   override val distance = _distance
    // }
    case object aldousBroder extends AldousBroder[N, C, G] {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object wilsons extends Wilsons[N, C, G] {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object huntAndKill extends HuntAndKill[N, C, G] {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    case object recursiveBacktracker extends RecursiveBacktracker[N, C, G] {
      case object _linkage extends Linkage[N, C, G]
      override type LINKAGE = Linkage[N, C, G]
      override val linker = _linkage
      case object _distance extends Distance[N, C, G]
      override type DISTANCE = Distance[N, C, G]
      override val distance = _distance
    }
    val generator: Generator[N, C, G] = (request.algorithm match {
      // case Algorithm.BinaryTree => binaryTree
      // case Algorithm.Sidewinder => sidewinder
      case Algorithm.AldousBroder => aldousBroder 
      case Algorithm.Wilsons => wilsons
      case Algorithm.HuntAndKill => huntAndKill 
      case Algorithm.RecursiveBacktracker => recursiveBacktracker
      case a => throw new IllegalArgumentException(s"Unexpected algorithm [$a]")
    }).asInstanceOf[Generator[N, C, G]]
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
