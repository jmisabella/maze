package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

trait HuntAndKill extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE
  
  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    var visited: Seq[Coordinates] = Nil
    val (randomIndex1, seed1): (Int, RNG) = nextGrid.randomInt(nextGrid.size())
    nextGrid = nextGrid.copy(seed = seed1)
    var current: Cell = nextGrid.flatten()(randomIndex1)
    while (current != null) {
      // ****** HUNT ****** 
      val unvisitedNeighbors: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => !visited.contains(c.coords))
      if (unvisitedNeighbors.nonEmpty) {
        val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = nextGrid.copy(seed = seed2)
        var neighbor: Cell = unvisitedNeighbors(randomIndex2)
        current = current.copy(linked = current.linked ++ Set(neighbor.coords))
        neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
        nextGrid = nextGrid.set(current).set(neighbor)
        visited = visited ++ Seq(current.coords) ++ Seq(neighbor.coords)
        current = neighbor
      } else {
        // ****** KILL ****** 
        current = nextGrid.find(c => { 
          !visited.contains(c.coords) && 
          nextGrid.unlinkedNeighbors(c).count(c2 => !nextGrid.get(c2).linked.isEmpty && visited.contains(c2.coords)) > 0
        }).getOrElse(null)
        if (current != null) {
          val neighborsAlreadyVisited: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => visited.contains(c.coords))
          if (neighborsAlreadyVisited.length > 0) {
            val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(neighborsAlreadyVisited.length)
            nextGrid = nextGrid.copy(seed = seed2)
            var neighbor: Cell = neighborsAlreadyVisited(randomIndex2)
            current = current.copy(linked = current.linked ++ Set(neighbor.coords))
            neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
            nextGrid = nextGrid.set(current).set(neighbor)
            visited = visited ++ Seq(current.coords)
          } else {
            current = null
          }
        } 
      }
    }
    nextGrid
  }
}
