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
    val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
      run = run ++ Seq(cell)
      (cell.neighbors.north, cell.neighbors.east, nextGrid.randomBoolean()) match {
        case (None, None, (_, seed)) => {
          nextGrid = nextGrid.copy(seed = seed) 
          Nil
        }
        case (None, Some(east), (_, seed)) => { // cannot go north, close run and go east
          nextGrid = nextGrid.copy(seed = seed)
          run = Nil // clear current run, onto the next run
          linker.link(Seq(cell, nextGrid.get(east))) // go east
        }
        case (Some(north), None, (true, seed)) => { // cannot go east, randomly close run
          nextGrid = nextGrid.copy(seed = seed)
          val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
          val member = run(randomIndex)
          run = Nil // clear current run, onto the next run
          if (member.neighbors.north.isDefined) {
            linker.link(Seq(cell, member))
          } else {
            Nil
          }
        }
        case (Some(north), _, (false, seed)) => { // cannot go north, randomly coninue run eastward 
          nextGrid = nextGrid.copy(seed = seed)
          linker.link(Seq(cell, nextGrid.get(north))) // go north ?? TODO: is this correct as per Sidewinder algorithm?? 
        }
        case (Some(north), Some(east), (_, seed)) => { // continue run eastward
          nextGrid = nextGrid.copy(seed = seed)
          linker.link(Seq(cell, nextGrid.get(east))) // go east
        }
      }
    }
    nextGrid = nextGrid.unflatten(unflattened.flatten)
    // deal with any unreachable cells by linking them accordingly
    linkUnreachables(nextGrid)
  }

}