package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.{ Linkage, Distance }
import maze.utilities.RNG

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid
  def generate(x: Int, y: Int): Grid = generate(Grid(x, y))

  // // given grid, randomly removes a wall from any unreachable cells 
  // def deisolateCells(grid: Grid): Grid = {
  //   // var nextGrid: Grid = grid
  //   // val unreachables: Seq[Cell] = nextGrid.flatten().filter(cell => linker.reachable(nextGrid, 0, 0, cell.coords.x, cell.coords.y))
  //   // // println("UNREACHABLES: " + unreachables.mkString("\n")) 
  //   // for (unreachable <- unreachables) {
  //   //   val availableUnlinked: Seq[Coordinates]= unreachable.availableNeighbors.filter(c => !unreachable.linked.contains(c))
  //   //   if (availableUnlinked != Nil) {
  //   //     // println("AVAILABLE UNLINKED: " + availableUnlinked.mkString(", "))
  //   //     val (index, seed): (Int, RNG) = nextGrid.randomInt(availableUnlinked.length)
  //   //     val cell: Cell = nextGrid.get(availableUnlinked(index).x)(availableUnlinked(index).y)
  //   //     val linked: Seq[Cell] = linker.link(Seq(unreachable, cell))
  //   //     // println("LINKING TO HEAD: " + linked.head)
  //   //     nextGrid = nextGrid.set(linked.head)
  //   //     if (linked.length > 1) {
  //   //       // println("LINKING TO TAIL: " + linked.tail.head)
  //   //       nextGrid = nextGrid.set(linked.tail.head)
  //   //     }
  //   //   }
  //   // }
  //   // nextGrid
  //   var nextGrid: Grid = grid
  //   var unreachables: Seq[Cell] = nextGrid.flatten.filter(c => !linker.reachable(nextGrid, 0, 0, c.coords.x, c.coords.y))
  //   while (!unreachables.isEmpty) {
  //     val unreached: Cell = unreachables.head
  //     val unlinkedNeighbors: Seq[Coordinates] = unreached.availableNeighbors().filter(c => !unreached.linked.contains(c)).toSeq
  //     val (i, nextSeed) = nextGrid.randomInt(unlinkedNeighbors.length)
  //     val cell: Cell = nextGrid.get(unlinkedNeighbors(i))
  //     val linkedCells: Seq[Cell] = linker.link(Seq(unreached, cell))
  //     nextGrid = nextGrid.copy(seed = nextSeed)
  //     for (linked <- linkedCells) {
  //       nextGrid = nextGrid.set(linked)
  //     }
  //     unreachables = nextGrid.flatten.filter(c => !linker.reachable(nextGrid, 0, 0, c.coords.x, c.coords.y))
  //   }
  //   nextGrid
  // }

}
