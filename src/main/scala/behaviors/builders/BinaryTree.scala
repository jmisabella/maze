package maze.behaviors.builders

import maze.classes.{ SquareNeighbors, SquareGrid, SquareCell, Coordinates }
import maze.behaviors.{ Linkage, INeighbors, ICell, IGrid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// BinaryTree algorithm only works with Square maze type
trait BinaryTree[N <: INeighbors, C <: ICell, G <: IGrid[C]] extends Generator[N, C, G] {

  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE

  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: SquareGrid = grid.asInstanceOf[SquareGrid] // to keep track of next random seeds
    val unflattened: Seq[Seq[SquareCell]] = for (cell <- grid.asInstanceOf[SquareGrid].flatten()) yield {
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
          linker.link(Seq(cell, nextGrid.cells(neighbor.y)(neighbor.x)).map(_.asInstanceOf[C])).asInstanceOf[Seq[SquareCell]]
          // linker.link(cell, nextGrid.cells(neighbor.y)(neighbor.x), nextGrid)
        }
      }
    }
    nextGrid.unflatten(unflattened.flatten)
  }

}
