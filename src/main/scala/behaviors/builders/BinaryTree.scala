package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.utilities.RNG

trait BinaryTree {

  type LINKAGE <: Linkage
  val linker: LINKAGE

  def build(grid: Grid): Grid = {
    var nextGrid: Grid = grid // to keep track of next random seeds
    val unflattenedWithEmpties: Seq[Seq[Option[Cell]]] = for (cell <- grid.flatten()) yield {
      val neighbors: Seq[Coordinates] = (cell.neighbors.north, cell.neighbors.east) match {
        case (None, None) => Nil
        case (Some(north), None) => Seq(north)
        case (None, Some(east)) => Seq(east)
        case (Some(north), Some(east)) => Seq(north, east)
      }
      neighbors match {
        case Nil => Seq(None) 
        case xs => {
          val (index, nextSeed): (Int, RNG) = nextGrid.seed.boundedPositiveInt(neighbors.length)
          nextGrid = nextGrid.copy(seed = nextSeed) // we made a random move, update grid's seed to the next seed
          val neighbor: Coordinates = neighbors(index)
          linker.link(Seq(cell, nextGrid.get(neighbor))).map(Some(_))
        }
      }
    }
    nextGrid.unflatten(unflattenedWithEmpties.flatten.filter(_.isDefined).map(_.get).toList)
  }

}
