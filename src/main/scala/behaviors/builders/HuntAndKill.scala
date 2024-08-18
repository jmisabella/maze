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
    val (randomIndex1, seed1): (Int, RNG) = nextGrid.randomInt(nextGrid.size())
    nextGrid = nextGrid.copy(seed = seed1)
    var current: Cell = nextGrid.flatten()(randomIndex1)
    while (current != null) {
      val unvisitedNeighbors1: Seq[Cell] = nextGrid.unlinkedNeighbors(current)
      if (unvisitedNeighbors1.nonEmpty) {
        val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors1.length)
        nextGrid = nextGrid.copy(seed = seed2)
        var neighbor: Cell = unvisitedNeighbors1(randomIndex2)
        current = current.copy(linked = current.linked ++ Set(neighbor.coords))
        neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
        nextGrid = nextGrid.set(current).set(neighbor)
        current = neighbor
      } else {
        current = null
        nextGrid.flatten().find { cell =>
          val visitedNeighbors: Seq[Cell] = nextGrid.linkedNeighbors(cell)
          cell.linked.isEmpty && visitedNeighbors.nonEmpty
        } match {
          case Some(cell) => 
            current = cell
            val visitedNeighbors: Seq[Cell] = nextGrid.linkedNeighbors(current)
            val (randomIndex3, seed3): (Int, RNG) = nextGrid.randomInt(visitedNeighbors.length)
            var neighbor: Cell = visitedNeighbors(randomIndex3)
            current = current.copy(linked = current.linked ++ Set(neighbor.coords))
            neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
            nextGrid = nextGrid.set(current).set(neighbor)
          case None =>
        }
      }
    }
    nextGrid
  }
}
