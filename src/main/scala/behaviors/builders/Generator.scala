package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates, MazeRequest, Algorithm }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Sidewinder, AldousBroder, Wilsons, HuntAndKill }

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid

  def generate(width: Int, height: Int, start: Coordinates, goal: Coordinates): Grid = generate(Grid(height, width, start, goal))

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
    case object aldousBroder extends AldousBroder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object wilsons extends Wilsons {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object huntAndKill extends HuntAndKill {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    case object recursiveBacktracker extends RecursiveBacktracker {
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
      case Algorithm.AldousBroder => aldousBroder 
      case Algorithm.Wilsons => wilsons
      case Algorithm.HuntAndKill => huntAndKill 
      case Algorithm.RecursiveBacktracker => recursiveBacktracker
      case a => throw new IllegalArgumentException(s"Unexpected algorithm [$a]")
    }
    generator.distance.pathTo( 
      generator.distance.distances(
        generator.generate(request.width, request.height, request.start, request.goal)
        , request.start.x
        , request.start.y)
      , request.start.x
      , request.start.y 
      , request.goal.x
      , request.goal.y)
  }

}
