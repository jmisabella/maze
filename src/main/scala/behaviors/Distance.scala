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

  def distances(grid: Grid, cell: Cell): Map[Coordinates, Int] = {
    var distances: Map[Coordinates, Int] = Map(cell.coords -> 0)
    var frontier: Seq[Cell] = Seq(cell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[Cell] = Nil
      for (c <- frontier) {
        for (linked <- c.linked) {
          if (!distances.keySet.contains(linked)) {
            distances = distances + (linked -> (distances.get(c.coords).getOrElse(0) + 1))
            newFrontier = newFrontier ++ Seq(grid.get(linked))
          }
        }
      }
      frontier = newFrontier
    }
    distances
  }
  def showDistances(grid: Grid, x: Int, y: Int): Grid = {
    val dist: Map[Coordinates, Int] = distances(grid, x, y)
    val withDinstances: Seq[Cell] = grid.cells.flatten.map(c => c.copy(value = pad(dist.get(c.coords).getOrElse(" ").toString(), ' ', 3))).toSeq
    grid.unflatten(withDinstances)
  }
  def showDistances(grid: Grid, coords: Coordinates): Grid = showDistances(grid, coords.x, coords.y)

  def distances(grid: Grid, x: Int, y: Int): Map[Coordinates, Int] = distances(grid, grid.get(x)(y))
  def distances(grid: Grid, coords: Coordinates): Map[Coordinates, Int] = distances(grid, grid.get(coords))

}