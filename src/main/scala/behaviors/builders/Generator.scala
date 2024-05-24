package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.{ Linkage, Distance }

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid
  def generate(x: Int, y: Int): Grid = generate(Grid(x, y))

  // given grid, randomly removes a wall from any unreachable cells 
  def linkUnreachables(grid: Grid): Grid = {
    def deisolate(grid: Grid): Grid = {
      var nextGrid: Grid = grid
      var unreachables: Seq[Cell] = nextGrid.flatten.filter(c => !nextGrid.reachable(0, 0, c.coords.x, c.coords.y))
      while (!unreachables.isEmpty) {
        val unreached: Cell = unreachables.head
        val unlinkedNeighbors: Seq[Coordinates] = unreached.availableNeighbors().filter(c => !unreached.linked.contains(c)).toSeq
        val (i, nextSeed) = nextGrid.randomInt(unlinkedNeighbors.length)
        val cell: Cell = nextGrid.get(unlinkedNeighbors(i))
        val linkedCells: Seq[Cell] = linker.link(Seq(unreached, cell))
        nextGrid = nextGrid.copy(seed = nextSeed)
        for (linked <- linkedCells) {
          nextGrid = nextGrid.set(linked)
        }
        unreachables = nextGrid.flatten.filter(c => !nextGrid.reachable(0, 0, c.coords.x, c.coords.y))
      }
      nextGrid
    }
    // deisolate(deisolate(deisolate(grid)))
    // deisolate(deisolate(grid))
    deisolate(grid)
  }

}
