package maze.behaviors.builders

import maze.classes.{ Coordinates, Cell, Grid }
import maze.classes.MazeType._
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import scala.reflect.ClassTag

trait AldousBroder extends Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE
  
  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    var cells: Seq[Cell] = grid.flatten() 
    var unvisited: Int = grid.size() - 1
    val (randomCellIndex, seed1): (Int, RNG)  = nextGrid.randomInt(cells.length)
    var cell: Cell = cells(randomCellIndex)
    nextGrid = nextGrid.set(seed = seed1) 
    while (unvisited > 0) {
      val neighbors: Seq[Coordinates]= cell.neighbors()
      val (randomNeighborIndex, seed2): (Int, RNG) = nextGrid.randomInt(neighbors.length)
      nextGrid = nextGrid.set(seed = seed2)
      var neighbor: Cell = cells.filter(c => c.coords == neighbors(randomNeighborIndex)).head
      if (neighbor.linked.isEmpty) {
        val linked: Seq[Cell] = linker.link(cell, neighbor, bidi=true)
        val (c1, c2): (Cell, Cell) = (linked.head, linked.tail.head)
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
    // nextGrid.unflatten(cells)
    nextGrid.unflatten(cells).linkUnreachables()
  }

}


