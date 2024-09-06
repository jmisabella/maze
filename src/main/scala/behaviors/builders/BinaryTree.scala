package maze.behaviors.builders

import maze.classes.{ SquareNeighbors, SquareGrid, SquareCell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

// BinaryTree algorithm only works with Square maze type
trait BinaryTree extends Generator[SquareNeighbors, SquareCell, SquareGrid] {

  type LINKAGE <: Linkage[SquareNeighbors, SquareCell, SquareGrid]
  val linker: LINKAGE

  override def generate(grid: SquareGrid): SquareGrid = {
    var nextGrid: SquareGrid = grid // to keep track of next random seeds
    val unflattened: Seq[Seq[SquareCell]] = for (cell <- grid.flatten()) yield {
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
          linker.link(Seq(cell, nextGrid.cells(neighbor.y)(neighbor.x)))
        }
      }
    }
    nextGrid.unflatten(unflattened.flatten)
  }

}
