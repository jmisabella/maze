package maze.classes

import maze.classes.{ Coordinates, Neighbors }
import maze.classes.Direction._
import play.api.libs.json.Json

case class Cell(
  coords: Coordinates, 
  neighbors: Neighbors = Neighbors(), 
  linked: Set[Coordinates] = Set(),
  distance: Int = 0,
  isStart: Boolean = false,
  isGoal: Boolean = false,
  onSolutionPath: Boolean = false, 
  visited: Boolean = false,
  value: String = "   "
  // coords: Coordinates, 
  // neighbors: Neighbors = Neighbors(), 
  // linked: Set[Coordinates] = Set(),
  // visited: Boolean = false,
  // value: String = "   "
) {
  def availableNeighbors(): Seq[Coordinates] = (neighbors.north, neighbors.east, neighbors.south, neighbors.west) match {
    // 4 
    case (Some(n), Some(e), Some(s), Some(w)) => Seq(n, e, s, w)
    // 3 
    case (Some(n), Some(e), Some(s), None) => Seq(n, e, s)
    case (Some(n), Some(e), None, Some(w)) => Seq(n, e, w)
    case (Some(n), None, Some(s), Some(w)) => Seq(n, s, w)
    case (None, Some(e), Some(s), Some(w)) => Seq(e, s, w)
    // 2 
    case (Some(n), Some(e), None, None) => Seq(n, e)
    case (Some(n), None, Some(s), None) => Seq(n, s)
    case (Some(n), None, None, Some(w)) => Seq(n, w)
    case (None, Some(e), Some(s), None) => Seq(e, s)
    case (None, Some(e), None, Some(w)) => Seq(e, w)
    case (None, None, Some(s), Some(w)) => Seq(s, w)
    // 1
    case (Some(n), None, None, None) => Seq(n)
    case (None, Some(e), None, None) => Seq(e)
    case (None, None, Some(s), None) => Seq(s)
    case (None, None, None, Some(w)) => Seq(w)
    // 0 
    case (None, None, None, None) => Nil
  }
  def isLinked(cell: Cell, bidi: Boolean = true): Boolean = bidi match {
    case false => linked.contains(cell.coords)
    case true => linked.contains(cell.coords) && cell.linked(this.coords)
  }
  def isLinked(coords: Coordinates): Boolean = linked.contains(coords)

  def isLinked(direction: Direction): Boolean = direction match {
    case North => neighbors.north.isDefined && isLinked(neighbors.north.get)
    case East => neighbors.east.isDefined && isLinked(neighbors.east.get)
    case South => neighbors.south.isDefined && isLinked(neighbors.south.get)
    case West => neighbors.west.isDefined && isLinked(neighbors.west.get)
  }

  def padRight(s: String, c: Char, n: Int): String = s.padTo(n, c).mkString
  def padLeft(s: String, c: Char, n: Int): String = n match {
    case 0 => s
    case x if (x < 0) => s
    case _ => padRight(s, c, n).split(s).tail.mkString + s
  }
  // evenly pad left and right; left has 1 extra padding in case of an odd length 
  def pad(s:String, c: Char, n:Int): String = {
    val left = (n - s.length) / 2
    val right = n - left - s.length
    c.toString * left + s + c.toString * right
  }

  override def toString(): String = {
    val linkedCells: Seq[String] = 
      (linked.contains(neighbors.north.getOrElse(Coordinates(-1, -1))),
       linked.contains(neighbors.east.getOrElse(Coordinates(-1, -1))),
       linked.contains(neighbors.south.getOrElse(Coordinates(-1, -1))),
       linked.contains(neighbors.west.getOrElse(Coordinates(-1, -1))),
      ) match {
        case (true, true, true, true) => Seq("north","east","south","west")
        case (true, true, true, false) => Seq("north","east","south")
        case (true, true, false, true) => Seq("north","east","west")
        case (true, false, true, true) => Seq("north","south","west")
        case (false, true, true, true) => Seq("east","south","west")
        case (true, true, false, false) => Seq("north","east")
        case (true, false, true, false) => Seq("north","south")
        case (true, false, false, true) => Seq("north","west")
        case (false, true, true, false) => Seq("east","south")
        case (false, true, false, true) => Seq("east","west")
        case (false, false, true, true) => Seq("south","west")
        case (true, false, false, false) => Seq("north")
        case (false, true, false, false) => Seq("east")
        case (false, false, true, false) => Seq("south")
        case (false, false, false, true) => Seq("west")
        case (false, false, false, false) => Nil
      }
    // (Json.obj(
    //   "coords" -> coords,
    //   // "neighbors" -> neighborCells,
    //   "linked" -> linkedCells,
    //   "visited" -> visited,
    //   "value" -> value.trim()
    // )).toString()
  (Json.obj(
      "coords" -> coords,
      "linked" -> linkedCells,
      "distance" -> distance,
      "isStart" -> isStart,
      "isGoal" -> isGoal, 
      "onSolutionPath" -> onSolutionPath//,
      // "visited" -> visited,
      // "value" -> value.trim()
    )).toString()}
}

object Cell {
  def apply(row: Int, column: Int): Cell = Cell(coords = Coordinates(row, column)) // TODO: which order is correct ???
  // def apply(row: Int, column: Int): Cell = Cell(coords = Coordinates(column, row)) // TODO: which order is correct ???
  
  def apply(row: Int, column: Int, start: Coordinates, goal: Coordinates): Cell = {
    val coordinates: Coordinates = Coordinates(row, column) // TODO: which order is correct ???
    // val coordinates: Coordinates = Coordinates(column, row) // TODO: which order is correct ???
    Cell(coords = coordinates, isStart = coordinates == start, isGoal = coordinates == goal)
  }
 
  implicit def ordering [A <: Cell]: Ordering[A] = Ordering.by(_.coords)
}
