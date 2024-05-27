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
    // val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
    var rowNum: Int = 0
    for (row <- grid.cells) {
      var colNum: Int = 0
      for (originalCell <- row) {
        val cell: Cell = nextGrid.get(originalCell.coords)
        run = run ++ Seq(cell)
        val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
        nextGrid = nextGrid.copy(seed = seed) 
        (cell.neighbors.north, cell.neighbors.east, randomOutcome) match {
          case (None, None, _) => {
            println(s"row $rowNum, col $colNum: upper-right corner, nowhere to go")
            run = Nil // clear current run, onto the next run
            // Nil
          }
          case (Some(north), Some(east), false) => { // go eastward, do not close the current run 
            println(s"row $rowNum, col $colNum: EAST (happy path)")
            for (c <- linker.link(Seq(cell, nextGrid.get(east)))) {
              nextGrid = nextGrid.set(c)
            }
          }
          case (None, Some(east), _) => { // cannot go north
            println(s"row $rowNum, col $colNum: EAST (top row)")
            for (c <- linker.link(Seq(cell, nextGrid.get(east)))) { // go east
              nextGrid = nextGrid.set(c)
            }
          }
          case (_, None, _) => { // cannot go east, close run and randomly choose cell from current run from which to move north 
            println(s"row $rowNum, col $colNum: NORTH (last column)")
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            val member = run(randomIndex)
            println("CURRENT RUN MEMBERS: " + run.mkString(", ")) 
            println("RANDOM MEMBER COORDS: " + member.coords)
            println("RANDOM MEMBER'S NORTHERN : " + member.neighbors.north)
            println("RANDOM MEMBER'S LINKED: " + member.linked.mkString(", "))
            run = Nil // clear current run, onto the next run
            if (member.neighbors.north.isDefined) {
              // for (c <- linker.link(Seq(cell, member))) {
              for (c <- linker.link(Seq(member, nextGrid.get(member.neighbors.north.get)))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
          case (Some(north), _, true) => { // coint toss is heads: close run and randomly chose one cell from current run from which to move north
            println(s"row $rowNum, col $colNum: NORTH (random close-out)")
            val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
            val member = run(randomIndex)
            println("CURRENT RUN MEMBERS: " + run.mkString(", ")) 
            println("RANDOM MEMBER COORDS: " + member.coords)
            println("RANDOM MEMBER'S NORTHERN : " + member.neighbors.north)
            println("RANDOM MEMBER'S LINKED: " + member.linked.mkString(", "))
            // for (c <- run) {
            //   // ??? 
            //   nextGrid = nextGrid.set(c)
            // }
            run = Nil // clear current run, onto the next run
            if (member.neighbors.north.isDefined) {
              // for (c <- linker.link(Seq(cell, member))) {
              // TODO: somehow this is clearing other cells' linked eastward cells from this run
              for (c <- linker.link(Seq(member, nextGrid.get(member.neighbors.north.get)))) {
                nextGrid = nextGrid.set(c)
              }
            }
          }
        }
        colNum += 1 
        println("HHH")
        println(nextGrid)
      }
      rowNum += 1
    }
    nextGrid
    // nextGrid = nextGrid.unflatten(unflattened.flatten)
    // nextGrid
  }
  
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

  // override def generate(grid: Grid): Grid = {
  //   var nextGrid: Grid = grid // to keep track of next random seeds
  //   var leftCells: Seq[Cell] = Nil
  //   // for (row <- grid.cells.toSeq) {
  //   for (row <- grid.reverseRows().toSeq) {
  //     for (cell <- row.toSeq.sorted) {
  //       leftCells = leftCells ++ Seq(cell)
  //       val (toss, seed) = nextGrid.randomBoolean()
  //       nextGrid = nextGrid.copy(seed = seed)
  //       val closeOut = !cell.neighbors.east.isDefined || (cell.neighbors.north.isDefined && toss)
  //       if (closeOut) {
  //         val (randomIndex, seed)  = nextGrid.randomInt(leftCells.length)
  //         nextGrid = nextGrid.copy(seed = seed)
  //         val member = leftCells(randomIndex)
  //         leftCells = Nil // clear current run
  //         if (member.neighbors.north.isDefined) {
  //           for (cell <- linker.link(Seq(member, nextGrid.get(member.neighbors.north.get)))) {
  //             println(cell.linked) 
  //             nextGrid = nextGrid.set(cell)
  //           }
  //         } //else if (member.neighbors.east.isDefined) {
  //         //   for (cell <- linker.link(Seq(cell, nextGrid.get(member.neighbors.east.get)))) {
  //         //     println(cell.linked) 
  //         //     nextGrid = nextGrid.set(cell)
  //         //   }
  //         // }
  //       } else {
  //         if (cell.neighbors.east.isDefined) {
  //           for (cell <- linker.link(Seq(cell, nextGrid.get(cell.neighbors.east.get)))) {
  //             println(cell.linked) 
  //             nextGrid = nextGrid.set(cell)
  //           }
  //         }
  //       }
  //       println(nextGrid.filter(c => !c.linked.isEmpty))
  //       println(nextGrid)
  //     }
  //   }
  //   // for (row <- nextGrid.reverseRows().toSeq) {
  //   // // for (row <- grid.cells.toSeq) {
  //   //   val southUnreachables = row.filter(c => c.neighbors.south.isDefined && c.linked == Set())
  //   //   // val southUnreachables = row.filter(c => c.neighbors.south.isDefined && linker.reachable(nextGrid, 0, 0, c.coords.x, c.coords.y))
  //   //   for (cell <- southUnreachables) {
  //   //     for (cell <- linker.link(Seq(cell, nextGrid.get(cell.neighbors.south.get)))) {
  //   //       nextGrid = nextGrid.set(cell)
  //   //     }
  //   //   }
  //   // } 
  //   nextGrid
  // }

  // override def generate(grid: Grid): Grid = {
  //   var nextGrid: Grid = grid // to keep track of next random seeds
  //   var run: Seq[Cell] = Nil
  //   val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
  //   // val unflattened: Seq[Seq[Cell]] = for (cell <- grid.reverseRows().toSeq.flatMap(c => c)) yield {
  //     // run = run ++ Seq(cell)
  //     // val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
  //     // nextGrid = nextGrid.copy(seed = seed)
  //     // val shouldCloseOut: Boolean = 
  //     //   !cell.neighbors.east.isDefined || // at eastern boundary
  //     //     (cell.neighbors.north.isDefined && randomOutcome) // or not at northern boundary and "coin was flipped as heads" 
  //     // if (shouldCloseOut) {
  //     //   val (randomIndex, seed)  = nextGrid.randomInt(run.length)
  //     //   nextGrid = nextGrid.copy(seed = seed)
  //     //   val member = run(randomIndex)
  //     //   run = Nil // clear current run, onto the next run
  //     //   if (member.neighbors.north.isDefined) {
  //     //     linker.link(Seq(cell, nextGrid.get(member.neighbors.north.get)))
  //     //   } else {
  //     //     // linker.link(Seq(cell, member))
  //     //     Nil // ???
  //     //   }
  //     // } else {
  //     //   if (cell.neighbors.east.isDefined) {
  //     //     linker.link(Seq(cell, nextGrid.get(cell.neighbors.east.get)))
  //     //   } else {
  //     //     Nil
  //     //   }
  //     // } 
  //     run = run ++ Seq(cell)
  //     val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
  //     nextGrid = nextGrid.copy(seed = seed)
  //     val shouldCloseOut: Boolean = 
  //       !cell.neighbors.east.isDefined || // at eastern boundary
  //         (cell.neighbors.north.isDefined && randomOutcome) // or not at northern boundary and "coin was flipped as heads" 
  //     if (shouldCloseOut) {
  //       val (randomIndex, seed)  = nextGrid.randomInt(run.length)
  //       nextGrid = nextGrid.copy(seed = seed)
  //       val member = run(randomIndex)
  //       run = Nil // clear current run, onto the next run
  //       if (member.neighbors.north.isDefined) {
  //         linker.link(Seq(cell, nextGrid.get(member.neighbors.north.get)))
  //       } else {
  //         // linker.link(Seq(cell, member))
  //         Nil // ???
  //       }
  //     } else {
  //       if (cell.neighbors.east.isDefined) {
  //         linker.link(Seq(cell, nextGrid.get(cell.neighbors.east.get)))
  //       } else {
  //         Nil
  //       }
  //     } 
  //     // run = run ++ Seq(cell)
  //     // val (randomOutcome, seed): (Boolean, RNG) = nextGrid.randomBoolean()
  //     // nextGrid = nextGrid.copy(seed = seed)
  //     // val shouldCloseOut: Boolean = 
  //     //   !cell.neighbors.east.isDefined || // at eastern boundary
  //     //     (cell.neighbors.north.isDefined && randomOutcome) // or not at northern boundary and "coin was flipped as heads" 
  //     // if (shouldCloseOut) {
  //     //   val (randomIndex, seed)  = nextGrid.randomInt(run.length)
  //     //   nextGrid = nextGrid.copy(seed = seed)
  //     //   val member = run(randomIndex)
  //     //   run = Nil // clear current run, onto the next run
  //     //   if (member.neighbors.north.isDefined) {
  //     //     linker.link(Seq(cell, nextGrid.get(member.neighbors.north.get)))
  //     //   } else {
  //     //     // linker.link(Seq(cell, member))
  //     //     Nil // ???
  //     //   }
  //     // } else {
  //     //   if (cell.neighbors.east.isDefined) {
  //     //     linker.link(Seq(cell, nextGrid.get(cell.neighbors.east.get)))
  //     //   } else {
  //     //     Nil
  //     //   }
  //     // }
  //   }
  //   nextGrid = nextGrid.unflatten(unflattened.flatten.sorted)
  //   //// for Sidewinder only, I have unreachable cells which are made reachable if last column is linked
  //   // val lastColumn: Seq[Cell] = (for (row <- 0 until nextGrid.rows) yield nextGrid.cells(row)(nextGrid.columns - 1)).toSeq
  //   // for ((c1, c2) <- lastColumn zip lastColumn.drop(1)) {
  //   //   val linked = linker.link(Seq(c1, c2))
  //   //   nextGrid = nextGrid.set(linked.head)
  //   //   if (linked.length > 1) {
  //   //     nextGrid = nextGrid.set(linked.tail.head)
  //   //   }
  //   // }
  //   nextGrid
  // }

  //   override def generate(grid: Grid): Grid = {
  //   var nextGrid: Grid = grid // to keep track of next random seeds
  //   var run: Seq[Cell] = Nil
  //   val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
  //     run = run ++ Seq(cell)
  //     (cell.neighbors.north, cell.neighbors.east, nextGrid.randomBoolean()) match {
  //       case (None, None, (_, seed)) => {
  //         nextGrid = nextGrid.copy(seed = seed) 
  //         Nil
  //       }
  //       case (None, Some(east), (_, seed)) => { // cannot go north, close run and go east
  //         nextGrid = nextGrid.copy(seed = seed)
  //         run = Nil // clear current run, onto the next run
  //         linker.link(Seq(cell, nextGrid.get(east))) // go east
  //       }
  //       case (Some(north), None, (true, seed)) => { // cannot go east, randomly close run
  //         nextGrid = nextGrid.copy(seed = seed)
  //         val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
  //         nextGrid = nextGrid.copy(seed = nextSeed)
  //         val member = run(randomIndex)
  //         run = Nil // clear current run, onto the next run
  //         if (member.neighbors.north.isDefined) {
  //           linker.link(Seq(cell, member))
  //         } else {
  //           Nil
  //         }
  //       }
  //       // //// after removing this section, Sidewinder no longer has unreachable cells, however Scala complains that not all permutations are checked... 
  //       // case (Some(north), _, (false, seed)) => { // cannot go north, randomly coninue run eastward 
  //       //   nextGrid = nextGrid.copy(seed = seed)
  //       //   linker.link(Seq(cell, nextGrid.get(north))) // go north ?? TODO: is this correct as per Sidewinder algorithm?? 
  //       // }
  //       case (Some(north), Some(east), (_, seed)) => { // continue run eastward
  //         nextGrid = nextGrid.copy(seed = seed)
  //         // linker.link(Seq(cell, nextGrid.get(east))) // go east
  //         linker.link(Seq(cell, nextGrid.get(north))) // go north
  //       }
  //       ///// ??????
  //       case (_, _, (_, seed)) => { // continue run eastward
  //         Nil
  //       }
  //       // case (Some(north), None, (_, seed)) => { // continue run eastward
  //       //   nextGrid = nextGrid.copy(seed = seed)
  //       //   val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
  //       //   nextGrid = nextGrid.copy(seed = nextSeed)
  //       //   val member = run(randomIndex)
  //       //   run = Nil // clear current run, onto the next run
  //       //   // linker.link(Seq(cell, nextGrid.get(east))) // go east
  //       //   linker.link(Seq(cell, member)) // go  ???
  //       // }
  //     }
  //   }
  //   nextGrid = nextGrid.unflatten(unflattened.flatten)
  //   //// for Sidewinder only, I have unreachable cells which are made reachable if last column is linked
  //   // val lastColumn: Seq[Cell] = (for (row <- 0 until nextGrid.rows) yield nextGrid.cells(row)(nextGrid.columns - 1)).toSeq
  //   // for ((c1, c2) <- lastColumn zip lastColumn.drop(1)) {
  //   //   val linked = linker.link(Seq(c1, c2))
  //   //   nextGrid = nextGrid.set(linked.head)
  //   //   if (linked.length > 1) {
  //   //     nextGrid = nextGrid.set(linked.tail.head)
  //   //   }
  //   // }
  //   nextGrid
  // }

  // override def generate(grid: Grid): Grid = {
  //   var nextGrid: Grid = grid // to keep track of next random seeds
  //   var run: Seq[Cell] = Nil
  //   val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
  //     run = run ++ Seq(cell)
  //     (cell.neighbors.north, cell.neighbors.east, nextGrid.randomBoolean()) match {
  //       case (None, None, (_, seed)) => {
  //         nextGrid = nextGrid.copy(seed = seed) 
  //         Nil
  //       }
  //       case (None, Some(east), (_, seed)) => { // cannot go north, close run and go east
  //         nextGrid = nextGrid.copy(seed = seed)
  //         run = Nil // clear current run, onto the next run
  //         linker.link(Seq(cell, nextGrid.get(east))) // go east
  //       }
  //       case (Some(north), None, (true, seed)) => { // cannot go east, randomly close run
  //         nextGrid = nextGrid.copy(seed = seed)
  //         val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
  //         nextGrid = nextGrid.copy(seed = nextSeed)
  //         val member = run(randomIndex)
  //         run = Nil // clear current run, onto the next run
  //         if (member.neighbors.north.isDefined) {
  //           linker.link(Seq(cell, member))
  //         } else {
  //           Nil
  //         }
  //       }
  //       // //// after removing this section, Sidewinder no longer has unreachable cells, however Scala complains that not all permutations are checked... 
  //       // case (Some(north), _, (false, seed)) => { // cannot go north, randomly coninue run eastward 
  //       //   nextGrid = nextGrid.copy(seed = seed)
  //       //   linker.link(Seq(cell, nextGrid.get(north))) // go north ?? TODO: is this correct as per Sidewinder algorithm?? 
  //       // }
  //       case (Some(north), Some(east), (_, seed)) => { // continue run eastward
  //         nextGrid = nextGrid.copy(seed = seed)
  //         // linker.link(Seq(cell, nextGrid.get(east))) // go east
  //         linker.link(Seq(cell, nextGrid.get(north))) // go north
  //       }
  //       ///// ??????
  //       case (_, _, (_, seed)) => { // continue run eastward
  //         Nil
  //       }
  //       // case (Some(north), None, (_, seed)) => { // continue run eastward
  //       //   nextGrid = nextGrid.copy(seed = seed)
  //       //   val (randomIndex, nextSeed)  = nextGrid.randomInt(run.length)
  //       //   nextGrid = nextGrid.copy(seed = nextSeed)
  //       //   val member = run(randomIndex)
  //       //   run = Nil // clear current run, onto the next run
  //       //   // linker.link(Seq(cell, nextGrid.get(east))) // go east
  //       //   linker.link(Seq(cell, member)) // go  ???
  //       // }
  //     }
  //   }
  //   nextGrid = nextGrid.unflatten(unflattened.flatten)
  //   //// for Sidewinder only, I have unreachable cells which are made reachable if last column is linked
  //   // val lastColumn: Seq[Cell] = (for (row <- 0 until nextGrid.rows) yield nextGrid.cells(row)(nextGrid.columns - 1)).toSeq
  //   // for ((c1, c2) <- lastColumn zip lastColumn.drop(1)) {
  //   //   val linked = linker.link(Seq(c1, c2))
  //   //   nextGrid = nextGrid.set(linked.head)
  //   //   if (linked.length > 1) {
  //   //     nextGrid = nextGrid.set(linked.tail.head)
  //   //   }
  //   // }
  //   nextGrid
  // }

}