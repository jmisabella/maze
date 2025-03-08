package maze.behaviors.builders

import maze.classes.Coordinates
import maze.classes.{ Cell, Grid }
import maze.classes.direction.SquareDirection._
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// BinaryTree algorithm only works with Square maze type
trait BinaryTree extends Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE

  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    for (y <- 0 until grid.height) {
      for (x <- 0 until grid.width) {
        val current: Cell = nextGrid.get(x, y)
        // randomly decide to carve north or east 
        val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.set(seed)
        if (y > 0 && (x == 0 || randomOutcome)) {
          // carve north
          nextGrid = linker.link(current, nextGrid.get(current.neighbors(North).head), nextGrid)
        } else if (x > 0) {
          nextGrid = linker.link(current, nextGrid.get(current.neighbors(West).head), nextGrid)
        }
      }
    }    
    nextGrid
  }

}