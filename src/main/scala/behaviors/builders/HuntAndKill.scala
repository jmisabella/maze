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
      val unvisitedNeighbors: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => !visited.contains(c.coords))
      if (unvisitedNeighbors.nonEmpty) { // HUNT
        val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = nextGrid.copy(seed = seed2)
        var neighbor: Cell = unvisitedNeighbors(randomIndex2)
        println(s"LINKING $current TO $neighbor")
        current = current.copy(linked = current.linked ++ Set(neighbor.coords))
        neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
        nextGrid = nextGrid.set(current).set(neighbor)
        visited = visited ++ Seq(current.coords)
        current = neighbor
      } else { // KILL
        // current = nextGrid.find(c => c.linked.isEmpty && c.neighborCoords().count(c => !grid.get(c).linked.isEmpty) > 0).getOrElse(null)
        // current = nextGrid.find(c =>  c.linked.isEmpty && nextGrid.unlinkedNeighbors(c).length > 0).getOrElse(null)
        // current = nextGrid.find(c => c.linked.isEmpty && nextGrid.unlinkedNeighbors(c).count(c2 => !nextGrid.get(c2).linked.isEmpty) > 0).getOrElse(null)
        current = nextGrid.find(c => !visited.contains(c.coords) && nextGrid.unlinkedNeighbors(c).count(c2 => !nextGrid.get(c2).linked.isEmpty && visited.contains(c2.coords)) > 0).getOrElse(null)
        if (current == null) { 
          return nextGrid
        }
        val neighborsAlreadyVisited: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => /*!nextGrid.get(c).linked.isEmpty &&*/ visited.contains(c.coords))
        // val neighborsAlreadyVisited: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => nextGrid.get(c).linked.isEmpty)
        if (neighborsAlreadyVisited.length > 0) {
          val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(neighborsAlreadyVisited.length)
          nextGrid = nextGrid.copy(seed = seed2)
          var neighbor: Cell = neighborsAlreadyVisited(randomIndex2)
          current = current.copy(linked = current.linked ++ Set(neighbor.coords))
          neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
          nextGrid = nextGrid.set(current).set(neighbor)
          // visited = visited ++ Seq(neighbor.coords)
          visited = visited ++ Seq(current.coords)
          // current = neighbor
        } else {
          current = null
        }

      }
    }
    nextGrid
  }
}
