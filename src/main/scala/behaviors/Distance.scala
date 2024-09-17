package maze.behaviors

// import maze.classes.{ Coordinates, Neighbors, Cell, Grid }
import maze.behaviors.{ Neighbors, Cell, Grid}
import maze.classes.{ Coordinates }

import scala.reflect.ClassTag

trait Distance[N <: Neighbors, C <: Cell, G <: Grid[C]] {
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
  def distances(grid: G, startCell: C): Map[Coordinates, Int] = {
    var distances: Map[Coordinates, Int] = Map(startCell.coords -> 0)
    var frontier: Seq[C] = Seq(startCell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[C] = Nil
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
  def getDistances(grid: G, startX: Int, startY: Int): Map[Coordinates, Int] = distances(grid, grid.cells(startY)(startX))
  def getDistances(grid: G, startCoords: Coordinates): Map[Coordinates, Int] = distances(grid, grid.cells(startCoords.y)(startCoords.x))
  def distances(grid: G, startX: Int, startY: Int)(implicit ct2: ClassTag[C]): G = {
    val dist: Map[Coordinates, Int] = getDistances(grid, startX, startY)
    val withDistances: Seq[C] = grid.cells.flatten.map(c => 
      Cell.instantiate[N, C](cell = c, distance = dist.get(c.coords).getOrElse(0), onSolutionPath = dist.get(c.coords).isDefined, value = pad(dist.get(c.coords).getOrElse(" ").toString(), ' ', 3))
      // c.copy(
      //   value = pad(dist.get(c.coords).getOrElse(" ").toString(), ' ', 3),
      //   onSolutionPath = dist.get(c.coords).isDefined, 
      //   distance = dist.get(c.coords).getOrElse(0)
      // )
      ).toSeq
    grid.unflatten[C, G](withDistances)
  }
  def distances(grid: G, startCoords: Coordinates)(implicit ct: ClassTag[C]): G = distances(grid, startCoords.x, startCoords.y)

  def getLongestPath(grid: G): Map[Coordinates, Int] = {
    val distances: Map[Coordinates, Int] = getDistances(grid, 0, 0)
    val (newStart, _): (Coordinates, Int) = distances.maxBy(_._2) 
    val newDistances: Map[Coordinates, Int] = getDistances(grid, newStart) 
    val (goal, _): (Coordinates, Int) = newDistances.maxBy(_._2) 
    getPathTo(grid, newStart.x, newStart.y, goal.x, goal.y) 
  }
  def longestPath(grid: G, overrideChar: Option[Char] = None)(implicit ct: ClassTag[C]): G = {
    val path: Map[Coordinates, String] = overrideChar match {
      case None => getLongestPath(grid).map(kv => kv._1 -> kv._2.toString()).toMap
      case Some(c) => getLongestPath(grid).map(kv => kv._1 -> c.toString()).toMap
    }
    val withDistances: Seq[C] = grid.cells.flatten.map(c => 
      Cell.instantiate[N, C](
        cell = c, 
        distance = try { path.get(c.coords).getOrElse("0").toInt } catch { case _: java.lang.NumberFormatException => 0 },
        onSolutionPath = path.get(c.coords).isDefined, 
        value = pad(path.get(c.coords).getOrElse(" ").toString(), ' ', 3))
      // c.copy(
      //   value = pad(path.get(c.coords).getOrElse(" ").toString(), ' ', 3),
      //   onSolutionPath = path.get(c.coords).isDefined, 
      //   distance = try {
      //     path.get(c.coords).getOrElse("0").toInt
      //   } catch {
      //     case _: java.lang.NumberFormatException => 0
      //   })
    ).toSeq
    grid.unflatten[C, G](withDistances)
  }
  def getPathTo(grid: G, startX: Int, startY: Int, goalX: Int, goalY: Int): Map[Coordinates, Int] = {
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
  def pathTo(grid: G, startX: Int, startY: Int, goalX: Int, goalY: Int, overrideChar: Option[Char] = None)(implicit ct: ClassTag[C]): G = {
    val shortestPath: Map[Coordinates, String] = overrideChar match {
      case None => getPathTo(grid, startX, startY, goalX, goalY).map(kv => kv._1 -> kv._2.toString()).toMap
      case Some(c) => getPathTo(grid, startX, startY, goalX, goalY).map(kv => kv._1 -> c.toString()).toMap
    }
    val distances: Map[Coordinates, Int] = getDistances(grid, startX, startY)
    val withDistances: Seq[C] = grid.cells.flatten.map(c => 
      Cell.instantiate[N, C](
        cell = c, 
        distance = try { distances.get(c.coords).getOrElse(grid.width * grid.height) } catch { case _: java.lang.NumberFormatException => grid.width * grid.height },
        onSolutionPath = shortestPath.get(c.coords).isDefined, 
        value = pad(shortestPath.get(c.coords).getOrElse(" ").toString(), ' ', 3))
    
      // c.copy(
      //   value = pad(shortestPath.get(c.coords).getOrElse(" ").toString(), ' ', 3),
      //   onSolutionPath = shortestPath.get(c.coords).isDefined, 
      //   distance = try {
      //     distances.get(c.coords).getOrElse(grid.columns * grid.rows)
      //   } catch {
      //     case _: java.lang.NumberFormatException => grid.columns * grid.rows
      //   })
      ).toSeq
    grid.unflatten[C, G](withDistances)
  }
  def pathTo(grid: G, startCoords: Coordinates, goalCoords: Coordinates)(implicit ct: ClassTag[C]): G = pathTo(grid, startCoords.x, startCoords.y, goalCoords.x, goalCoords.y)

}