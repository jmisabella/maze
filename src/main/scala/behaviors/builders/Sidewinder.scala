package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.{ RectangleGrid, SquareCell, Coordinates }
import maze.behaviors.{ Linkage, Cell, Grid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// Sidewinder algorithm only works with Square maze type
trait Sidewinder[C <: Cell, G <: Grid[C]] extends Generator[C, G] {

  type LINKAGE <: Linkage[C, G]
  val linker: LINKAGE
  
  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    for (y <- 0 until grid.height) {
      var run: List[C] = Nil
      for (x <- 0 until grid.width) {
        val current: C = nextGrid.get(x, y)
        run = run ++ Seq(current)
        val atEasternWall: Boolean = !(current.coords.x < grid.width - 1)
        val atNorthernWall: Boolean = current.coords.y == 0 
        val (randomOutcome, seed1): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.set(seed1)
        val shouldCloseOut: Boolean = atEasternWall || (randomOutcome && !atNorthernWall)
        if (shouldCloseOut) {
          val (randomIndex, seed2): (Int, RNG) = nextGrid.randomInt(run)
          nextGrid = nextGrid.set(seed2)
          val random: C = run(randomIndex)
          if (random.coords.y > 0) {
            nextGrid = linker.link(random, nextGrid.get(random.neighbors("north")), nextGrid)
            run = Nil // clear run after closing out the eastward dig
          }
        } else if (!atEasternWall) {
          nextGrid = linker.link(current, nextGrid.get(current.neighbors("east")), nextGrid)
        }
      }
    }
    linker.repairUniDirectonalLinks(nextGrid)
  }

}