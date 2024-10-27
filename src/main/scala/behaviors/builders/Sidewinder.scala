package maze.behaviors.builders

import maze.classes.{ Coordinates, Cell, Grid }
// import maze.classes.cell.SquareCell
import maze.classes.direction.SquareDirection._
import maze.classes.grid.SquareGrid
// import maze.behaviors.{ Linkage, Cell, Grid }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// Sidewinder algorithm only works with Square maze type
trait Sidewinder extends Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE
  
  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    for (y <- 0 until grid.height) {
      var run: List[Cell] = Nil
      for (x <- 0 until grid.width) {
        val current: Cell = nextGrid.get(x, y)
        run = run ++ Seq(current)
        val atEasternWall: Boolean = !(current.coords.x < grid.width - 1)
        val atNorthernWall: Boolean = current.coords.y == 0 
        val (randomOutcome, seed1): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.set(seed1)
        val shouldCloseOut: Boolean = atEasternWall || (randomOutcome && !atNorthernWall)
        if (shouldCloseOut) {
          val (randomIndex, seed2): (Int, RNG) = nextGrid.randomInt(run)
          nextGrid = nextGrid.set(seed2)
          val random: Cell = run(randomIndex)
          if (random.coords.y > 0) {
            nextGrid = linker.link(random, nextGrid.get(random.neighbors(North).head), nextGrid)
            run = Nil // clear run after closing out the eastward carving
          }
        } else if (!atEasternWall) {
          nextGrid = linker.link(current, nextGrid.get(current.neighbors(East).head), nextGrid)
        }
      }
    }
    linker.repairUniDirectonalLinks(nextGrid)
  }

}