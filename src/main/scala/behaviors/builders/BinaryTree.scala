package maze.behaviors.builders

import maze.classes.{ SquareNeighbors, RectangleGrid, SquareCell, Coordinates }
import maze.behaviors.{ Linkage, Neighbors, Cell, Grid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// BinaryTree algorithm only works with Square maze type
trait BinaryTree[N <: Neighbors, C <: Cell, G <: Grid[C]] extends Generator[N, C, G] {

  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE

  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    for (cell <- grid.flatten()) {
      val c1 = nextGrid.get(cell.coords)
      val neighbors: Seq[Coordinates] = (c1.asInstanceOf[SquareCell].neighbors.north, c1.asInstanceOf[SquareCell].neighbors.east) match {
        case (None, None) => Nil
        case (Some(north), None) => Seq(north)
        case (None, Some(east)) => Seq(east)
        case (Some(north), Some(east)) => Seq(north, east)
      }
      neighbors match {
        case Nil => Seq(c1)
        case xs => {
          val (index, nextSeed): (Int, RNG) = nextGrid.randomInt(neighbors.length)
          nextGrid = nextGrid.set(seed = nextSeed) // we made a random move, update grid's seed to the next seed
          val neighbor: Coordinates = neighbors(index)
          nextGrid = nextGrid.set(linker.link(Seq(c1, nextGrid.cells(neighbor.y)(neighbor.x))))
        }
      }
    }
    nextGrid
  }

}