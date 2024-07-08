package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

trait BinaryTree extends Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE

  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid // to keep track of next random seeds
    val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
      val neighbors: Seq[Coordinates] = (cell.neighbors.north, cell.neighbors.east) match {
        case (None, None) => Nil
        case (Some(north), None) => Seq(north)
        case (None, Some(east)) => Seq(east)
        case (Some(north), Some(east)) => Seq(north, east)
      }
      neighbors match {
        case Nil => Seq(cell)
        case xs => {
          val (index, nextSeed): (Int, RNG) = nextGrid.randomInt(neighbors.length)
          nextGrid = nextGrid.copy(seed = nextSeed) // we made a random move, update grid's seed to the next seed
          val neighbor: Coordinates = neighbors(index)
          // linker.link(Seq(cell, nextGrid.get(neighbor)))
          linker.link(Seq(cell, nextGrid.cells(neighbor.x)(neighbor.y)))
        }
      }
    }
    nextGrid.unflatten(unflattened.flatten)
  }

}
