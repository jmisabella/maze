package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.MazeType._
import maze.behaviors.{ Linkage, Cell, Grid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import scala.reflect.ClassTag

trait AldousBroder[C <: Cell, G <: Grid[C]] extends Generator[C, G] {

  type LINKAGE <: Linkage[C, G]
  val linker: LINKAGE
  
  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    var cells: Seq[C] = grid.flatten() 
    var unvisited: Int = grid.size() - 1
    val (randomCellIndex, seed1): (Int, RNG)  = nextGrid.randomInt(cells.length)
    var cell: C = cells(randomCellIndex)
    nextGrid = nextGrid.set[C, G](seed = seed1) 
    while (unvisited > 0) {
      val neighbors: Seq[Coordinates]= cell.neighbors()
      val (randomNeighborIndex, seed2): (Int, RNG) = nextGrid.randomInt(neighbors.length)
      nextGrid = nextGrid.set[C, G](seed = seed2)
      var neighbor: C = cells.filter(c => c.coords == neighbors(randomNeighborIndex)).head
      if (neighbor.linked.isEmpty) {
        val linked: Seq[C] = linker.link(cell, neighbor, bidi=true)
        val (c1, c2): (C, C) = (linked.head, linked.tail.head)
        cells = cells.map(c => c.coords match {
          case c1.coords => c1
          case c2.coords => c2
          case _ => c
        })
        neighbor = c2
        unvisited -= 1
      }
      cell = neighbor
    }
    nextGrid.unflatten(cells)
  }

}


