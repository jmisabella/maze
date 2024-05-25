package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

trait Sidewinder extends Generator {

  type LINKAGE <: Linkage
  val linker: LINKAGE
  
  // override def generate(grid: Grid): Grid = {
  //   var nextGrid: Grid = grid // to keep track of next random seeds
  //   for (row <- (0 until nextGrid.rows)) {
  //     var current: Int = 0 
  //     for (col <- (0 until nextGrid.columns)) {
  //       val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
  //       nextGrid = nextGrid.copy(seed = seed)
  //       if (row > 0 && (col == nextGrid.columns - 1 || randomOutcome)) {
  //         val (randomIndex, nextSeed): (Int, RNG) = nextGrid.randomInt(col - current + 1)
  //         nextGrid = nextGrid.copy(seed = nextSeed)
  //         val passageCol: Int = current + randomIndex
  //         val north: Cell = grid.get(row - 1)(passageCol)
  //         val south: Cell = grid.get(row)(passageCol)
  //         val linked: Seq[Cell] = linker.link(Seq(north, south))
  //         nextGrid = nextGrid.set(linked.head)
  //         nextGrid = nextGrid.set(linked.tail.head)
  //         current = col + 1 
  //       } else if (col == grid.columns - 1) {
  //         val west = nextGrid.get(row)(col)
  //         val east = nextGrid.get(row)(col + 1)
  //         val linked: Seq[Cell] = linker.link(Seq(east, west))
  //         nextGrid = nextGrid.set(linked.head)
  //         nextGrid = nextGrid.set(linked.tail.head)
  //       }
  //     }
  //   }
  //   nextGrid
  // }

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
            // // TODO: should we link east here if possible??
            // if (member.neighbors.east.isDefined) { // ??
            //   linker.link(Seq(cell, nextGrid.get(member.neighbors.east.get.x)(member.neighbors.east.get.y))) // ??
            // } else { // ??
            //   Nil // ??
            // } // ??
            Nil
          }
        }
        //// after removing this section, Sidewinder no longer has unreachable cells, however Scala complains that not all permutations are checked... 
        // case (Some(north), _, (false, seed)) => { // cannot go north, randomly coninue run eastward 
        //   nextGrid = nextGrid.copy(seed = seed)
        //   linker.link(Seq(cell, nextGrid.get(north))) // go north ?? TODO: is this correct as per Sidewinder algorithm?? 
        // }
        case (Some(north), Some(east), (_, seed)) => { // continue run eastward
          nextGrid = nextGrid.copy(seed = seed)
          linker.link(Seq(cell, nextGrid.get(east))) // go east
        }
      }
    }
    nextGrid = nextGrid.unflatten(unflattened.flatten)
    //// for Sidewinder only, I have unreachable cells which are made reachable if last column is linked
    // val lastColumn: Seq[Cell] = (for (row <- 0 until nextGrid.rows) yield nextGrid.cells(row)(nextGrid.columns - 1)).toSeq
    // for ((c1, c2) <- lastColumn zip lastColumn.drop(1)) {
    //   val linked = linker.link(Seq(c1, c2))
    //   nextGrid = nextGrid.set(linked.head)
    //   if (linked.length > 1) {
    //     nextGrid = nextGrid.set(linked.tail.head)
    //   }
    // }
    nextGrid
  }

}