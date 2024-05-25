package maze.behaviors

import maze.classes.{ Coordinates, Neighbors, Cell, Grid }

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
  // TODO: this seems to lead to an infinite loop, need to fix
  def distances(grid: Grid, startCell: Cell): Map[Coordinates, Int] = {
    var distances: Map[Coordinates, Int] = Map(startCell.coords -> 0)
    var frontier: Seq[Cell] = Seq(startCell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[Cell] = Nil
      for (c <- frontier) {
        for (linked <- c.linked) {
          if (!distances.keySet.contains(linked)) {
            distances = distances + (linked -> (distances.get(c.coords).getOrElse(0) + 1))
            // distances = distances + (linked -> (distances.get(c.coords).getOrElse(-99999999) + 1))
            newFrontier = newFrontier ++ Seq(grid.get(linked))
          }
        }
      }
      frontier = newFrontier
    }
    distances
    // distances.filter(entry => entry._2 >= 0) // remove unlinked cells
  }
  def distances(grid: Grid, startX: Int, startY: Int): Map[Coordinates, Int] = distances(grid, grid.get(startX)(startY))
  def distances(grid: Grid, startCoords: Coordinates): Map[Coordinates, Int] = distances(grid, grid.get(startCoords))
  def showDistances(grid: Grid, startX: Int, startY: Int): Grid = {
    val dist: Map[Coordinates, Int] = distances(grid, startX, startY)
    val withDinstances: Seq[Cell] = grid.cells.flatten.map(c => c.copy(value = pad(dist.get(c.coords).getOrElse(" ").toString(), ' ', 3))).toSeq
    grid.unflatten(withDinstances)
  }
  def showDistances(grid: Grid, startCoords: Coordinates): Grid = showDistances(grid, startCoords.x, startCoords.y)
  // TODO: test
  // TODO: this no longer seems to work, is seemingly leading to an infinite loop 
  def pathTo(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int): Map[Coordinates, Int] = {
    val dist: Map[Coordinates, Int] = distances(grid, startX, startY)
    var current: Coordinates = Coordinates(goalX, goalY)
    var breadcrumbs: Map[Coordinates, Int] = Map(current -> dist(current))
    while (current != Coordinates(startX, startY)) {
      for (neighbor <- grid.get(current).linked) {
        if (dist(neighbor) < dist(current)) {
          breadcrumbs = breadcrumbs ++ Map(neighbor -> dist(neighbor))
          current = neighbor
        }
      }
    }
    breadcrumbs 
  }
  // TODO: this no longer seems to work, is seemingly leading to an infinite loop 
  def showPathTo(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int): Grid = {
    val shortestPath: Map[Coordinates, Int] = pathTo(grid, startX, startY, goalX, goalY)
    val withDinstances: Seq[Cell] = grid.cells.flatten.map(c => c.copy(value = pad(shortestPath.get(c.coords).getOrElse(" ").toString(), ' ', 3))).toSeq
    grid.unflatten(withDinstances)
  }
  def showPathTo(grid: Grid, startCoords: Coordinates, goalCoords: Coordinates): Grid = showPathTo(grid, startCoords.x, startCoords.y, goalCoords.x, goalCoords.y)

}