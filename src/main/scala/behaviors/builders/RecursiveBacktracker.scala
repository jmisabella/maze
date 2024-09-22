package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.MazeType._
import maze.behaviors.{ Linkage, Cell, Grid }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

import scala.reflect.ClassTag

trait RecursiveBacktracker[C <: Cell, G <: Grid[C]] extends Generator[C, G] {
  type LINKAGE <: Linkage[C, G]
  val linker: LINKAGE
  
  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    @annotation.tailrec
    def generateMaze(grid: G, stack: List[C], visited: Set[Coordinates]): G = stack match {
      case Nil =>
        grid // 
      case current :: rest =>
        var nextGrid: G = grid 
        val currentCell: C = nextGrid.get(current.coords)
        val unvisitedNeighbors: Seq[C] = nextGrid.unlinkedNeighbors(current).filter(c => !visited.contains(c.coords))
        if (unvisitedNeighbors.nonEmpty) {
          val (randomIndex, seed): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
          nextGrid = nextGrid.set[C, G](seed = seed)
          val neighbor: C = unvisitedNeighbors(randomIndex)
          nextGrid = linker.link(currentCell, neighbor, nextGrid)
          println(nextGrid.asci())
          generateMaze(nextGrid, neighbor :: stack, visited + neighbor.coords)
        } else {
          // backtrack if no unvisited neighbors
          generateMaze(grid, rest, visited)
        }
    }
    // initialize the stack and visited set with randomly selected first cell
    val (randomIndex, seed) = grid.randomInt(grid.size())
    val initialCell: C = grid.flatten()(randomIndex)
    generateMaze(grid.set[C, G](seed = seed), List(initialCell), Set(initialCell.coords))
  }
}
