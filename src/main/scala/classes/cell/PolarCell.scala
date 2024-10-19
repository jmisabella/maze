package maze.classes.cell

import maze.behaviors.Cell
import maze.classes.{ Coordinates, MazeType }
import maze.classes.MazeType._
import maze.classes.direction.PolarDirection._
import play.api.libs.json.Json

case class PolarCell (
  coords: Coordinates, 
  neighborsByDirection: Map[String, Coordinates] = Map(),
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  visited: Boolean = false,
  value: String = "   ",
  outward: Seq[Coordinates] = Nil // because polar cells can have more than 1 outward neighbor due to adaptive subdivision
) extends Cell() {

  override def mazeType: MazeType = Polar

  override def neighbors[D <: Enumeration#Value](direction: D): Seq[Coordinates] = direction match {
    case ClockWise => Seq(neighborsByDirection("ClockWise"))
    case CounterClockWise => Seq(neighborsByDirection("CounterClockWise"))
    case Inward => Seq(neighborsByDirection("Inward"))
    case Outward => Seq(neighborsByDirection("Outward")) // TODO: what about if more than 1 outward neighbor? 
    case d => throw new IllegalArgumentException(s"Unexpected direction [$d] for PolarCell; expecting one of [ClockWise, CounterClockWise, Inward, Outward]")
  }

  override def isLinked[PolarDirection](direction: PolarDirection): Boolean = direction match {
    case ClockWise => neighborsByDirection.get("ClockWise").isDefined && isLinkedCoords(neighborsByDirection.get("ClockWise"))
    case CounterClockWise => neighborsByDirection.get("CounterClockWise").isDefined && isLinkedCoords(neighborsByDirection.get("CounterClockWise"))
    case Inward => neighborsByDirection.get("Inward").isDefined && isLinkedCoords(neighborsByDirection.get("Inward"))
    case Outward => neighborsByDirection.get("Outward").isDefined && isLinkedCoords(neighborsByDirection.get("Outward"))
  }

  override def toString(): String = {
    val linkedCells: Seq[String] = 
      (linked.contains(neighborsByDirection.get("ClockWise").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("CounterClockWise").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("Inward").getOrElse(Coordinates(-1, -1))),
       linked.contains(neighborsByDirection.get("Outward").getOrElse(Coordinates(-1, -1))),
      ) match {
        case (true, true, true, true) => Seq("ClockWise","CounterClockWise","Inward","Outward")
        case (true, true, true, false) => Seq("ClockWise","CounterClockWise","Inward")
        case (true, true, false, true) => Seq("ClockWise","CounterClockWise","Outward")
        case (true, false, true, true) => Seq("ClockWise","Inward","Outward")
        case (false, true, true, true) => Seq("CounterClockWise","Inward","Outward")
        case (true, true, false, false) => Seq("ClockWise","CounterClockWise")
        case (true, false, true, false) => Seq("ClockWise","Inward")
        case (true, false, false, true) => Seq("ClockWise","Outward")
        case (false, true, true, false) => Seq("CounterClockWise","Inward")
        case (false, true, false, true) => Seq("CounterClockWise","Outward")
        case (false, false, true, true) => Seq("Inward","Outward")
        case (true, false, false, false) => Seq("ClockWise")
        case (false, true, false, false) => Seq("CounterClockWise")
        case (false, false, true, false) => Seq("Inward")
        case (false, false, false, true) => Seq("Outward")
        case (false, false, false, false) => Nil
      }
  (Json.obj(
      "coords" -> coords,
      "linked" -> linkedCells,
      "distance" -> distance,
      "isStart" -> isStart,
      "isGoal" -> isGoal, 
      "onSolutionPath" -> onSolutionPath//,
    )).toString()
  }
}

object PolarCell {
  def apply(x: Int, y: Int): PolarCell = PolarCell(coords = Coordinates(x, y))
}


