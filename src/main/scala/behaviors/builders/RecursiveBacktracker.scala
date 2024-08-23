package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

trait RecursiveBacktracker extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE
  
  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    val visited: ListBuffer[Coordinates] = new ListBuffer[Coordinates]()
    val (randomIndex1, seed1): (Int, RNG) = nextGrid.randomInt(nextGrid.size())
    nextGrid = nextGrid.copy(seed = seed1)
    var current: Cell = nextGrid.flatten()(randomIndex1)
    visited += current.coords
    val stack = Stack[Cell](current)
    while (stack.nonEmpty) {
      current = nextGrid.get(stack.top.coords)
      val unvisitedNeighbors: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => !visited.contains(c.coords))
      if (unvisitedNeighbors.nonEmpty) {
        val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = nextGrid.copy(seed = seed2)
        var neighbor: Cell = unvisitedNeighbors(randomIndex2)
        println("LINKING " + current.coords + " TO " + neighbor.coords)
        // current = current.copy(linked = current.linked ++ Set(neighbor.coords))
        // neighbor = neighbor.copy(linked = neighbor.linked ++ Set(current.coords))
        // nextGrid = nextGrid.set(current).set(neighbor)
        nextGrid = linker.link(current, neighbor, nextGrid)
        visited += neighbor.coords
        stack.push(neighbor) 
      } else {
        // backtrack if no unvisited neighbors
        stack.pop() // go backwards 1 cell by popping last visited cell from the stack
      }
    }    
    nextGrid
  }
}
