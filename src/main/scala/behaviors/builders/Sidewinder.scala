package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.{ SquareNeighbors, SquareGrid, SquareCell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

trait Sidewinder extends Generator[SquareNeighbors, SquareCell, SquareGrid] {

  type LINKAGE <: Linkage[SquareNeighbors, SquareCell, SquareGrid]
  val linker: LINKAGE

  override def generate(grid: SquareGrid): SquareGrid = {
    var nextGrid: SquareGrid = grid // to keep track of next random seeds
    var run: Seq[SquareCell] = Nil
    for (row <- grid.cells) {
      for (originalCell <- row) {
        val cell: SquareCell = nextGrid.cells(originalCell.coords.y)(originalCell.coords.x)
        run = run ++ Seq(cell)
        val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.copy(seed = seed) 
        (cell.neighbors.north, cell.neighbors.east, randomOutcome) match {
          case (None, None, _) => {
            run = Nil // clear current run, onto the next run
          }
          case (Some(north), Some(east), false) => { // go eastward, do not close the current run 
            for (c <- linker.link(Seq(cell, nextGrid.cells(east.y)(east.x)))) {
              nextGrid = nextGrid.set(c)
            }
          }
          case (None, Some(east), _) => { // cannot go north
            for (c <- linker.link(Seq(cell, nextGrid.cells(east.y)(east.x)))) { // go east
              nextGrid = nextGrid.set(c)
            }
          }
          case (_, None, _) => { // cannot go east, close run and randomly choose cell from current run from which to move north 
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            val rand: Coordinates = run(randomIndex).coords
            val member = nextGrid.cells(rand.y)(rand.x)
            run = Nil // clear current run, onto the next run
            if (member.neighbors.north.isDefined) {
              for (c <- linker.link(Seq(member, nextGrid.cells(member.neighbors.north.get.y)(member.neighbors.north.get.x)))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
          case (Some(north), _, true) => { // coin toss is heads: close run and randomly chose one cell from current run from which to move north
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            val rand: Coordinates = run(randomIndex).coords 
            val member = nextGrid.cells(rand.y)(rand.x)
            run = Nil // clear current run, onto the next run
            if (member.neighbors.north.isDefined) {
              for (c <- linker.link(Seq(member, nextGrid.cells(member.neighbors.north.get.y)(member.neighbors.north.get.x)))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
        }
      }
    }
    nextGrid
  }

}