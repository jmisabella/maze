package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.Linkage

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE
 
  def generate(grid: Grid): Grid

  def linkUnreachables(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    var dist = nextGrid.distances(0)(0)
    var unreachables: Seq[Cell] = nextGrid.flatten.filter(c => !dist.keySet.contains(c.coords))
    while (!unreachables.isEmpty) {
      val unreached: Cell = unreachables.head
      val unlinkedNeighbors: Seq[Coordinates] = unreached.availableNeighbors().filter(c => !unreached.linked.contains(c)).toSeq
      val (i, nextSeed) = nextGrid.randomInt(unlinkedNeighbors.length)
      val cell: Cell = nextGrid.get(unlinkedNeighbors(i))
      val linkedCells: Seq[Cell] = linker.link(Seq(unreached, cell))
      nextGrid = nextGrid.copy(seed = nextSeed)
      nextGrid = nextGrid.set(linkedCells.head)
      nextGrid = nextGrid.set(linkedCells.reverse.head)
      dist = nextGrid.distances(0)(0)
      unreachables = nextGrid.flatten().filter(c => !dist.keySet.contains(c.coords))
    }
    nextGrid
  }

}
