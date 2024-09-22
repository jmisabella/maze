package maze.behaviors.builders

import maze.classes.{ RectangleGrid, SquareCell, Coordinates }
import maze.behaviors.{ Linkage, Cell, Grid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// BinaryTree algorithm only works with Square maze type
trait BinaryTree[C <: Cell, G <: Grid[C]] extends Generator[C, G] {

  type LINKAGE <: Linkage[C, G]
  val linker: LINKAGE

  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    for (y <- 0 until grid.height) {
      for (x <- 0 until grid.width) {
        val current: C = nextGrid.get(x, y)
        // randomly decide to carve north or east 
        val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.set(seed)
        if (y > 0 && (x == 0 || randomOutcome)) {
          // carve north
          nextGrid = linker.link(current, nextGrid.get(current.neighborsByDirection("north")), nextGrid)
        } else if (x > 0) {
          nextGrid = linker.link(current, nextGrid.get(current.neighborsByDirection("west")), nextGrid)
        }
      }
    }    
    nextGrid
  }

}