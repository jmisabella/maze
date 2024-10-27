package maze.behaviors

// import maze.behaviors.{ Cell, Grid}
import maze.classes.{ Coordinates, Cell, Grid }
// import maze.classes.cell._
// import scala.reflect.ClassTag

trait Distance {
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
  def distances(grid: Grid, startCell: Cell): Map[Coordinates, Int] = {
    var distances: Map[Coordinates, Int] = Map(startCell.coords -> 0)
    var frontier: Seq[Cell] = Seq(startCell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[Cell] = Nil
      for (c <- frontier) {
        for (linked <- c.linked) {
          if (!distances.keySet.contains(linked)) {
            distances = distances + (linked -> (distances.get(c.coords).getOrElse(0) + 1))
            newFrontier = newFrontier ++ Seq(grid.cells(linked.y)(linked.x))
          }
        }
      }
      frontier = newFrontier
    }
    distances
  }
  def getDistances(grid: Grid, startX: Int, startY: Int): Map[Coordinates, Int] = distances(grid, grid.cells(startY)(startX))
  def getDistances(grid: Grid, startCoords: Coordinates): Map[Coordinates, Int] = distances(grid, grid.cells(startCoords.y)(startCoords.x))
  def distances(grid: Grid, startX: Int, startY: Int): Grid = {
    val dist: Map[Coordinates, Int] = getDistances(grid, startX, startY)
    val withDistances: Seq[Cell] = grid.cells.flatten.map(c => 
      Cell(cell = c, distance = dist.get(c.coords).getOrElse(0), onSolutionPath = dist.get(c.coords).isDefined, value = pad(dist.get(c.coords).getOrElse(" ").toString(), ' ', 3))
    ).toSeq
    grid.unflatten(withDistances)
  }
  def distances(grid: Grid, startCoords: Coordinates): Grid = distances(grid, startCoords.x, startCoords.y)

  def getLongestPath(grid: Grid): Map[Coordinates, Int] = {
    val distances: Map[Coordinates, Int] = getDistances(grid, 0, 0)
    val (newStart, _): (Coordinates, Int) = distances.maxBy(_._2) 
    val newDistances: Map[Coordinates, Int] = getDistances(grid, newStart) 
    val (goal, _): (Coordinates, Int) = newDistances.maxBy(_._2) 
    getPathTo(grid, newStart.x, newStart.y, goal.x, goal.y) 
  }
  def longestPath(grid: Grid, overrideChar: Option[Char] = None): Grid = {
    val path: Map[Coordinates, String] = overrideChar match {
      case None => getLongestPath(grid).map(kv => kv._1 -> kv._2.toString()).toMap
      case Some(c) => getLongestPath(grid).map(kv => kv._1 -> c.toString()).toMap
    }
    val withDistances: Seq[Cell] = grid.cells.flatten.map(c => 
      Cell(
        cell = c, 
        distance = try { path.get(c.coords).getOrElse("0").toInt } catch { case _: java.lang.NumberFormatException => 0 },
        onSolutionPath = path.get(c.coords).isDefined, 
        value = pad(path.get(c.coords).getOrElse(" ").toString(), ' ', 3))
    ).toSeq
    grid.unflatten(withDistances)
  }
  def getPathTo(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int): Map[Coordinates, Int] = {
    val dist: Map[Coordinates, Int] = getDistances(grid, startX, startY)
    var current: Coordinates = Coordinates(goalX, goalY)
    var breadcrumbs: Map[Coordinates, Int] = Map(current -> dist(current))
    while (current != Coordinates(startX, startY)) {
      for (neighbor <- grid.cells(current.y)(current.x).linked) {
        if (dist(neighbor) < dist(current)) {
          breadcrumbs = breadcrumbs ++ Map(neighbor -> dist(neighbor))
          current = neighbor
        }
      }
    }
    breadcrumbs 
  }
  def pathTo(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int, overrideChar: Option[Char] = None): Grid = {
    val shortestPath: Map[Coordinates, String] = overrideChar match {
      case None => getPathTo(grid, startX, startY, goalX, goalY).map(kv => kv._1 -> kv._2.toString()).toMap
      case Some(c) => getPathTo(grid, startX, startY, goalX, goalY).map(kv => kv._1 -> c.toString()).toMap
    }
    val distances: Map[Coordinates, Int] = getDistances(grid, startX, startY)
    val withDistances: Seq[Cell] = grid.cells.flatten.map(c => 
      Cell(
        cell = c, 
        distance = try { distances.get(c.coords).getOrElse(grid.width * grid.height) } catch { case _: java.lang.NumberFormatException => grid.width * grid.height },
        onSolutionPath = shortestPath.get(c.coords).isDefined, 
        value = pad(shortestPath.get(c.coords).getOrElse(" ").toString(), ' ', 3))
    
      ).toSeq
    grid.unflatten(withDistances)
  }
  def pathTo(grid: Grid, startCoords: Coordinates, goalCoords: Coordinates): Grid = pathTo(grid, startCoords.x, startCoords.y, goalCoords.x, goalCoords.y)

}