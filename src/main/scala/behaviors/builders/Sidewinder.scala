package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.{ SquareNeighbors, RectangleGrid, SquareCell, Coordinates }
import maze.behaviors.{ Linkage, Neighbors, Cell, Grid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// Sidewinder algorithm only works with Square maze type
trait Sidewinder[N <: Neighbors, C <: Cell, G <: Grid[C]] extends Generator[N, C, G] {

  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE
  
  //// TODO: because it's not a perfect maze, this implementation of Sidewinder seems to lead to infinite loop when determining solution path from Generator!
  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    for (y <- 0 until grid.height) {
      var run: List[C] = Nil
      for (x <- 0 until grid.width) {
        val current: C = nextGrid.get(x, y)
        run = current :: run

        // Randomly decide whether to carve a passage to the right or down
        val (randomOutcome, seed1): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.set(seed1)
        val carveRight: Boolean = x < grid.width - 1 && (y == grid.height - 1 || randomOutcome)
        if (carveRight) {
          nextGrid = linker.link(current, nextGrid.get(current.neighbors("east")), nextGrid)
        } else {
          // Carve downwards if not at the last row
          if (y < grid.height - 1 && run.nonEmpty) {
            val (randomIndex, seed2): (Int, RNG) = nextGrid.randomInt(run)
            nextGrid = nextGrid.set(seed2)
            val random: C = run(randomIndex)
            nextGrid = linker.link(random, nextGrid.get(random.neighbors("south")), nextGrid)
          }
          run = Nil // Reset the run
        }
      }
    }
    nextGrid.linkUnreachables()
  }

}