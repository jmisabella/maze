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
    @annotation.tailrec
    def generateMaze(grid: Grid, stack: List[Cell], visited: Set[Coordinates]): Grid = stack match {
      case Nil =>
        grid // 
      case current :: rest =>
        var nextGrid: Grid = grid 
        val currentCell: Cell = nextGrid.get(current.coords)
        val unvisitedNeighbors: Seq[Cell] = nextGrid.unlinkedNeighbors(current).filter(c => !visited.contains(c.coords))
        if (unvisitedNeighbors.nonEmpty) {
          val (randomIndex, seed): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
          nextGrid = nextGrid.copy(seed = seed)
          val neighbor: Cell = unvisitedNeighbors(randomIndex)
          nextGrid = linker.link(currentCell, neighbor, nextGrid)
          generateMaze(nextGrid, neighbor :: stack, visited + neighbor.coords)
        } else {
          // backtrack if no unvisited neighbors
          generateMaze(grid, rest, visited)
        }
    }
    // initialize the stack and visited set with randomly selected first cell
    val (randomIndex, seed) = grid.randomInt(grid.size())
    val initialCell: Cell = grid.flatten()(randomIndex)
    generateMaze(grid.copy(seed = seed), List(initialCell), Set(initialCell.coords))
  }
}
