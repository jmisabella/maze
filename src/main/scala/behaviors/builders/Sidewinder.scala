package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.{ SquareNeighbors, RectangleGrid, SquareCell, Coordinates }
import maze.behaviors.{ Linkage, Neighbors, Cell, Grid }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

// Sidewinder algorithm only works with Square maze type
trait Sidewinder[N <: Neighbors, C <: Cell, G <: Grid[C]] extends Generator[N, C, G] {

  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE

  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    var run: Seq[C] = Nil
    for (row <- grid.cells) {
      for (originalCell <- row) {
        val cell: C = nextGrid.cells(originalCell.coords.y)(originalCell.coords.x)
        run = run ++ Seq(cell)
        val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.set(seed = seed) 
        (cell.asInstanceOf[SquareCell].neighbors.north, cell.asInstanceOf[SquareCell].neighbors.east, randomOutcome) match {
          case (None, None, _) => {
            run = Nil // clear current run, onto the next run
          }
          case (Some(north), Some(east), false) => { // go eastward, do not close the current run 
            for (c <- linker.link(Seq(cell.asInstanceOf[C], nextGrid.cells(east.y)(east.x).asInstanceOf[C]))) {
              nextGrid = nextGrid.set(c)
            }
          }
          case (None, Some(east), _) => { // cannot go north
            for (c <- linker.link(Seq(cell.asInstanceOf[C], nextGrid.cells(east.y)(east.x).asInstanceOf[C]))) { // go east
              nextGrid = nextGrid.set(c)
            }
          }
          case (_, None, _) => { // cannot go east, close run and randomly choose cell from current run from which to move north 
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            val rand: Coordinates = run(randomIndex).coords
            val member = nextGrid.cells(rand.y)(rand.x)
            run = Nil // clear current run, onto the next run
            if (member.asInstanceOf[SquareCell].neighbors.north.isDefined) {
              for (c <- linker.link(Seq(member.asInstanceOf[C], nextGrid.cells(member.asInstanceOf[SquareCell].neighbors.north.get.y)(member.asInstanceOf[SquareCell].neighbors.north.get.x).asInstanceOf[C]))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
          case (Some(north), _, true) => { // coin toss is heads: close run and randomly chose one cell from current run from which to move north
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            val rand: Coordinates = run(randomIndex).coords 
            val member = nextGrid.cells(rand.y)(rand.x)
            run = Nil // clear current run, onto the next run
            if (member.asInstanceOf[SquareCell].neighbors.north.isDefined) {
              for (c <- linker.link(Seq(member.asInstanceOf[C], nextGrid.cells(member.asInstanceOf[SquareCell].neighbors.north.get.y)(member.asInstanceOf[SquareCell].neighbors.north.get.x).asInstanceOf[C]))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
        }
      }
    }
    nextGrid.asInstanceOf[G]
  }

}