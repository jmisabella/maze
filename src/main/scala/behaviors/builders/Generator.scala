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

  protected def linkUnreachables(grid: Grid): Grid = {
    // def deisolate(grid: Grid, startX: Int, startY: Int): Grid = {
    //   var nextGrid: Grid = grid
    //   var dist = distance.distances(nextGrid, startX, startY)
    //   var unreachables: Seq[Cell] = nextGrid.flatten.filter(c => !dist.keySet.contains(c.coords))
    //   while (!unreachables.isEmpty) {
    //     val unreached: Cell = unreachables.head
    //     val unlinkedNeighbors: Seq[Coordinates] = unreached.availableNeighbors().filter(c => !unreached.linked.contains(c)).toSeq
    //     val (i, nextSeed) = nextGrid.randomInt(unlinkedNeighbors.length)
    //     val cell: Cell = nextGrid.get(unlinkedNeighbors(i))
    //     val linkedCells: Seq[Cell] = linker.link(Seq(unreached, cell))
    //     nextGrid = nextGrid.copy(seed = nextSeed)
    //     nextGrid = nextGrid.set(linkedCells.head)
    //     nextGrid = nextGrid.set(linkedCells.reverse.head)
    //     dist = distance.distances(nextGrid, 0, 0)
    //     unreachables = nextGrid.flatten().filter(c => !dist.keySet.contains(c.coords))
    //   }
    //   nextGrid
    // }

    // 
    def deisolate(grid: Grid, startX: Int, startY: Int): Grid = {
      var nextGrid: Grid = grid
      var dist = distance.distances(nextGrid, startX, startY)
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
        dist = distance.distances(nextGrid, 0, 0)
        unreachables = nextGrid.flatten().filter(c => !dist.keySet.contains(c.coords))
      }
      nextGrid
    }
    deisolate(deisolate(grid, 0, 0), grid.rows - 1, grid.columns - 1)
  }

}
