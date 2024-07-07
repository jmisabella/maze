package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

trait Sidewinder extends Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE

  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid // to keep track of next random seeds
    var run: Seq[Cell] = Nil
    for (row <- grid.cells) {
      for (originalCell <- row) {
        // val cell: Cell = nextGrid.get(originalCell.coords)
        val cell: Cell = nextGrid.cells(originalCell.coords.x)(originalCell.coords.y)
        run = run ++ Seq(cell)
        val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.copy(seed = seed) 
        (cell.neighbors.north, cell.neighbors.east, randomOutcome) match {
          case (None, None, _) => {
            run = Nil // clear current run, onto the next run
          }
          case (Some(north), Some(east), false) => { // go eastward, do not close the current run 
            // for (c <- linker.link(Seq(cell, nextGrid.get(east)))) {
            for (c <- linker.link(Seq(cell, nextGrid.cells(east.x)(east.y)))) {
              nextGrid = nextGrid.set(c)
            }
          }
          case (None, Some(east), _) => { // cannot go north
            // for (c <- linker.link(Seq(cell, nextGrid.get(east)))) { // go east
            for (c <- linker.link(Seq(cell, nextGrid.cells(east.x)(east.y)))) { // go east
              nextGrid = nextGrid.set(c)
            }
          }
          case (_, None, _) => { // cannot go east, close run and randomly choose cell from current run from which to move north 
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            // val member = nextGrid.get(run(randomIndex).coords)
            val rand: Coordinates = run(randomIndex).coords
            val member = nextGrid.cells(rand.x)(rand.y)
            run = Nil // clear current run, onto the next run
            if (member.neighbors.north.isDefined) {
              // for (c <- linker.link(Seq(member, nextGrid.get(member.neighbors.north.get)))) {
              for (c <- linker.link(Seq(member, nextGrid.cells(member.neighbors.north.get.x)(member.neighbors.north.get.y)))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
          case (Some(north), _, true) => { // coin toss is heads: close run and randomly chose one cell from current run from which to move north
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            // val member = nextGrid.get(run(randomIndex).coords)
            val rand: Coordinates = run(randomIndex).coords 
            val member = nextGrid.cells(rand.x)(rand.y)
            run = Nil // clear current run, onto the next run
            if (member.neighbors.north.isDefined) {
              // for (c <- linker.link(Seq(member, nextGrid.get(member.neighbors.north.get)))) {
              for (c <- linker.link(Seq(member, nextGrid.cells(member.neighbors.north.get.x)(member.neighbors.north.get.y)))) {
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