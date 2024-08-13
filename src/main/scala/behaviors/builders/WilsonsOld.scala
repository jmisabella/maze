package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import maze.classes.Neighbors

import java.util.Random // TODO: remove

trait WilsonsOld extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE

  override def generate(grid: Grid): Grid = {
    var flattenedResults: Seq[Cell] = Nil
    var nextGrid: Grid = grid
    var unvisited = nextGrid.cells.flatten.toBuffer // Assuming grid.cells gives a 2D array of cells
    val random = new Random
    var i = 0
    // Start with a random cell
    val first = unvisited(random.nextInt(unvisited.length))
    // unvisited -= first // jmi ???
    var path = List(first) // jmi ???

    var visited: Set[Coordinates] = Set.empty[Coordinates]
    while (unvisited.nonEmpty) {
      if (i > 20) {
        throw new IllegalArgumentException("forcing exit")
      }
      i += 1
      // Pick a random cell from unvisited
      var cell = unvisited(random.nextInt(unvisited.length))
      
      // var visited: Set[Coordinates] = Set.empty[Coordinates]
      // visited += cell.coords // jmi ???
      // unvisited -= cell // jmi ???
      var break = false
      // Explore the path until we find a cell that is visited
      // while (!break && unvisited.map(_.coords).contains(cell.coords)) {
      while (!break && unvisited.map(_.coords).contains(cell.coords)) {
        // println("VISITED: " + visited.mkString(","))
        // println("CHECKING FOR COORDS: " + cell.coords)
        if (path.length >= 2 && visited.contains(cell.coords)) {
          println("VISITED ALREADY CONTAINS COORDS: " + cell.coords)
          println("PATH BEFORE TRUNCATION UP TO COORDS " + cell.coords + ": length = " + path.length + ", contents = " + path.mkString(","))
          path = path.take(path.map(_.coords).indexOf(cell.coords) + 1) // If found, truncate the path
          // println("PATH AFTER TRUNCATION: length = " + path.length + ", contents = " + path.mkString(","))
          cell = null
          break = true
        } else {
          // println("IS CELL NULL? " + cell == null) 
          // println("DOES PATH ALREADY CONTAIN CELL? " + path.map(_.coords).contains(cell.coords)) 
          if (cell != null && !path.map(_.coords).contains(cell.coords)) {
            println("ADDING COORDS " + cell.coords + " TO PATH")
            path = cell :: path
          }
          visited += cell.coords
        }
      }
      break = false
      println("DDDDDD")
      // throw new IllegalArgumentException("STOPPpPPPP 1")

      // Link the path and remove from unvisited
      // path.zip(path.tail).foreach { case (current, next) =>
      //   current.link(next)
      //   unvisited -= current
      // }
      //// TODO: issue seems to be that only 1 cell is ever in path, therefore these loopings by 2 cells are never yielding any results since length is only 1, not 2
      if (path.length > 1) {
        val linkedForwards = for (i <- 0 to path.length - 2) yield {
          val cell = path(i)
          val neighbor = path(i + 1)
          // unvisited = unvisited.filter(c => c.coords != cell.coords) 
          unvisited -= cell
          linker.link(cell, neighbor, bidi=true).head
        }
        // throw new IllegalArgumentException("STOPPpPPPP 2")
        val linkedBiDirectional = for (i <- 0 to linkedForwards.length - 2) yield {
          val reversed = linkedForwards.reverse
          val cell = reversed(i)
          val neighbor = reversed(i + 1)
          // unvisited = unvisited.filter(c => c.coords != cell.coords) 
          unvisited -= cell
          linker.link(cell, neighbor, bidi=true).head
        }
        flattenedResults = flattenedResults ++ linkedBiDirectional
        println("PATH LENGTH: " + path.length)
        println("PATH: " + path)
        println("LINKED FORWARDS LENGTH: " + linkedForwards.length)
        println("LINKED FORWARDS: " + linkedForwards.length)

        println("UNVISITED LENGTH: " + unvisited.length)
        println("VISITED LENGTH: " + visited.toSeq.length)
        println("RESETTING PATH")
        path = Nil
        println("RESETTING VISITED")
        visited = Set()
      } 
      // throw new IllegalArgumentException("STOPPpPPPP 3")
      // println("UNVISITED: " + unvisited)
      // throw new IllegalArgumentException("PATH LENGTH: " + path.length)
      // throw new IllegalArgumentException("LINKED FORWARDS LENGTH: " + linkedForwards.length)
      // throw new IllegalArgumentException("LINKED BI DIRECTIONAL LENGTH: " + linkedBiDirectional.length)
      // throw new IllegalArgumentException(flattenedResults.mkString(","))
    }
    nextGrid.unflatten(flattenedResults)
    // nextGrid 
  }
}



    // var flattenedResults: Seq[Cell] = Nil
    // var nextGrid: Grid = grid
    // var unvisited: Seq[Cell] = nextGrid.flatten()
    // var i = 0
    // val (randomFirstIndex, seed1): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
    // // println("UNVISITED: " + unvisited.mkString("|"))
    // println("UNVISITED INITIAL LENGTH: " + unvisited.length)
    // var first: Cell = unvisited(randomFirstIndex)
    // println("FIRST: " + first.coords)
    // nextGrid = nextGrid.copy(seed = seed1)
    // unvisited = unvisited.filter(c => c.coords != first.coords)
    // println("UNVISITED UPDATED LENGTH: " + unvisited.length)
    // // println("UNVISITED: " + unvisited.mkString("|"))
    // while (!unvisited.isEmpty) {
    //   // if (i > 100) {
    //   //   return nextGrid
    //   // }
    //   // i += 1 
    //   val (randomCellIndex1, seed2): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
    //   var cell: Cell = unvisited(randomCellIndex1)
    //   println("NEXT: " + cell.coords)
    //   nextGrid = nextGrid.copy(seed = seed2) 
    //   var path: Seq[Cell] = Seq(cell)
    //   while (unvisited.contains(cell)) {
    //     if (i > 20) {
    //       return nextGrid
    //     }
    //     i += 1 
    //     val (randomCellIndex2, seed3): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
    //     nextGrid = nextGrid.copy(seed = seed3) 
    //     cell = unvisited(randomCellIndex2)
    //     if (path.map(_.coords).contains(cell.coords)) {
    //       // loop erase
    //       println("AAAA")
    //       path = for (i <- 0 to path.map(c => c.coords).indexOf(cell.coords)) yield path(i) // TODO: until (exclusive) or to (inclusive) ???
    //       println("PATH: " + path.mkString("|"))
    //       unvisited = unvisited.filter(c => !path.contains(cell.coords))
    //       println("UNVISITED CONTAINS " + cell.coords + "? " + unvisited.contains(cell.coords))
    //       println("UNVISITED LENGTH: " + unvisited.length)
    //     } else {
    //       println("BBBBBB")
    //       path = path ++ Seq(cell)
    //     }
    //   }
    //   println("GOT FIRST PATH, LINKING CELLS...")
    //   // TODO: should this be to (inclusive) or until (exclusive) ???
    //   val linkedForwards = for (i <- 0 to path.length - 2) yield {
    //     val cell = path(i)
    //     val neighbor = path(i + 1)
    //     unvisited = unvisited.filter(c => c.coords != cell.coords) 
    //     linker.link(cell, neighbor, bidi=true).head
    //   }
    //   val linkedBiDirectional = for (i <- 0 to linkedForwards.length - 2) yield {
    //     val reversed = linkedForwards.reverse
    //     val cell = reversed(i)
    //     val neighbor = reversed(i + 1)
    //     unvisited = unvisited.filter(c => c.coords != cell.coords) 
    //     linker.link(cell, neighbor, bidi=true).head
    //   }
    //   flattenedResults = flattenedResults ++ linkedBiDirectional
    // }
    // nextGrid.unflatten(flattenedResults)
  // }
















  // def randomWalk(startCell: Cell, unvisited: Seq[Cell], random: RNG): (Seq[Cell], RNG) = {
  //   val path = scala.collection.mutable.ListBuffer[Cell]()
  //   var un = unvisited 
  //   var currentCell = startCell.copy(visited = true)
  //   var seed = random
  //   // var i = 0
  //   do {
  //     // if (i > 9999) {
  //     //   return (path.toSeq, seed)
  //     // }
  //     // i += 1
  //     println("CURRENT COORDS: " + currentCell.coords)
  //     println("VISITED: " + currentCell.visited)
  //     path += currentCell
  //     val (randomIndex, seed1): (Int, RNG) = random.boundedPositiveInt(currentCell.neighbors.toSeq.length)
  //     val filtered = un.filter(c => !path.contains(c) && c.coords == currentCell.neighbors.toSeq()(randomIndex))
  //     if (filtered.length == 0) {
  //       return (path.toSeq, seed)
  //     }
  //     val nextCell = filtered.head
  //     seed = seed1 
  //     if (!nextCell.visited) {
  //       println("CELL NOT YET VISITED")
  //       currentCell = nextCell.copy(visited = true)
  //       un = un.map(c => if (c.coords == currentCell.coords) c.copy(visited = true) else c)
  //     } else {
  //       println("BEAKING OUT OF LOOP")
  //       // If we hit a visited cell, break the loop
  //       currentCell = null
  //     }
  //   } while (currentCell != null)
  //   (path.toSeq, seed)
  // }

  // override def generate(grid: Grid): Grid = {
  //   // val x = getRandomPath(grid.get(0, 0), Nil, grid.seed)
  //   // grid
  //   var flattenedResults: Seq[Cell] = Nil
  //   var nextGrid: Grid = grid
  //   var seed: RNG = grid.seed 
  //   var unvisited: Seq[Cell] = nextGrid.flatten()
  //   var visited: Seq[Cell] = Nil
  //   // randomly visit the first cell by randomly removing it from unvisited list
  //   val (randomIndex, seed1): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
  //   val randomVisited: Cell = unvisited(randomIndex)
  //   unvisited = unvisited.filter(c => c.coords != randomVisited.coords)
  //   visited = visited ++ Seq(randomVisited)
  //   while (unvisited.length > 0) {
  //     val cell = unvisited.head
  //     // val (coordsPath, seed2): (Seq[Coordinates], RNG) = randomWalk(cell, unvisited, seed)
  //     // return nextGrid // TODO: remove
  //     val (coordsPath, seed2): (Seq[Cell], RNG) = randomWalk(cell, unvisited, seed)
  //     // return nextGrid // TODO: remove
  //     val path: Seq[Cell] = nextGrid.flatten().filter(c => coordsPath.contains(c.coords))
  //     // val (path, seed2): (Seq[Cell], RNG) = getRandomPath(cell, unvisited, seed)
  //     seed = seed2
  //     // unvisited = Nil
  //     val linkedForwards = for (i <- 0 to path.length - 2) yield {
  //       val cell = path(i)
  //       val neighbor = path(i + 1)
  //       unvisited = unvisited.filter(c => c.coords != cell.coords) 
  //       linker.link(cell, neighbor, bidi=true).head
  //     }
  //     val linkedBiDirectional = for (i <- 0 to linkedForwards.length - 2) yield {
  //       val reversed = linkedForwards.reverse
  //       val cell = reversed(i)
  //       val neighbor = reversed(i + 1)
  //       unvisited = unvisited.filter(c => c.coords != cell.coords) 
  //       linker.link(cell, neighbor, bidi=true).head
  //     }
  //     visited = visited ++ linkedBiDirectional
  //     // unvisited = Nil
  //   }
  //   grid.copy(seed = seed).unflatten(visited)
  // }
  
  // override def generate(grid: Grid): Grid = {
    
  //   ???
  // }
  // // Perform random walk to visit the cells
  // private def randomWalk(startCell: Cell, unvisited: scala.collection.mutable.Set[Cell], random: RNG): (Seq[Cell], RNG) = {
  //   val path = scala.collection.mutable.ListBuffer[Cell]()
  //   var currentCell = startCell.copy(visited = true)
  //   var seed = random 
  //   do {
  //     path += currentCell
  //     val (randomIndex, seed1): (Int, RNG) = random.boundedPositiveInt(currentCell.neighbors.toSeq.length)
  //     val nextCell = unvisited.filter(c => c.coords == currentCell.neighbors.toSeq()(randomIndex)).head
  //     seed = seed1 
  //     // Check if the next cell has been visited
  //     if (!nextCell.visited) {
  //       currentCell = nextCell.copy(visited = true)
  //     } else {
  //       // If we hit a visited cell, break the loop
  //       currentCell = null
  //     }
  //   } while (currentCell != null)

  //   // // Backtrack through the path to mark the connections
  //   // for (i <- path.length - 2 to 0 by -1) {
  //   //   connectCells(path(i), path(i + 1))
  //   // }
  //   (path.toSeq, seed)
  // }

  // def getRandomPath(startCell: Cell, unvisited: Seq[Cell], random: RNG, grid: Grid): (Seq[Coordinates], RNG) = {
  //   var path = Seq(startCell.coords)
  //   var seed = random 
  //   var coords: Coordinates = startCell.coords
  //   var i = 0
  //   println("START COORDS: " + coords)
  //   while (unvisited.map(_.coords).contains(coords)) {
  //   // while (true) {
  //     if (i > 9999) {
  //       return (path, seed)
  //     }
  //     i += 1
  //     val neighbors: Seq[Coordinates] = unvisited.filter(c => c.coords == coords).head.neighbors.toSeq
  //     println(neighbors.mkString(",")) 
  //     // val neighbors: Seq[Coordinates] = grid.get(coords).neighbors.toSeq
  //     val (randomIndex, seed1): (Int, RNG) = random.boundedPositiveInt(neighbors.length)
  //     seed = seed1
  //     // val cell: Cell = unvisited.filter(c => c.coords == neighbors(randomIndex)).head
  //     coords = neighbors(randomIndex)
  //     println("NEW COORDS: " + coords)
  //     println("DOES UNVISITED CONTAIN " + coords + "? " + unvisited.map(_.coords).contains(coords))
  //     println("UNVISITED: " + unvisited.map(_.coords).mkString("|"))
  //     if (!unvisited.map(_.coords).contains(coords)) {
  //     // if (unvisited.map(_.coords).contains(coords)) {
  //       println("AAAA")
  //       // found rest of the maze
  //       path = path ++ Seq(coords)
  //       return (path, seed)
  //     } else if (path.contains(coords)) {
  //       // println("BBBB")
  //       // intersect with self, remove loop from the path
  //       println("PREVIOUS PAtH: " + path.mkString("|"))
  //       path = for (i <- 0 to path.indexOf(coords)) yield path(i)
  //       println("NEW PAtH: " + path.mkString("|"))
  //       // return (path, seed) // TODO: remove
  //     } else {
  //       // println("CCCC")
  //       path = path ++ Seq(coords)
  //       // return (path, seed) // TODO: remove
  //     }
  //   }
  //   (path, seed)
  // }
  
  // override def generate(grid: Grid): Grid = {
  //   // val x = getRandomPath(grid.get(0, 0), Nil, grid.seed)
  //   // grid
  //   var flattenedResults: Seq[Cell] = Nil
  //   var nextGrid: Grid = grid
  //   var seed: RNG = grid.seed 
  //   var unvisited: Seq[Cell] = nextGrid.flatten()
  //   var visited: Seq[Cell] = Nil
  //   // randomly visit the first cell by randomly removing it from unvisited list
  //   val (randomIndex, seed1): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
  //   val randomVisited: Cell = unvisited(randomIndex)
  //   unvisited = unvisited.filter(c => c.coords != randomVisited.coords)
  //   visited = visited ++ Seq(randomVisited)
  //   while (unvisited.length > 0) {
  //     val cell = unvisited.head
  //     val (coordsPath, seed2): (Seq[Coordinates], RNG) = getRandomPath(cell, unvisited, seed, nextGrid)
  //     return nextGrid // TODO: remove
  //     val path: Seq[Cell] = nextGrid.flatten().filter(c => coordsPath.contains(c.coords))
  //     // val (path, seed2): (Seq[Cell], RNG) = getRandomPath(cell, unvisited, seed)
  //     seed = seed2
  //     // unvisited = Nil
  //     val linkedForwards = for (i <- 0 to path.length - 2) yield {
  //       val cell = path(i)
  //       val neighbor = path(i + 1)
  //       unvisited = unvisited.filter(c => c.coords != cell.coords) 
  //       linker.link(cell, neighbor, bidi=true).head
  //     }
  //     val linkedBiDirectional = for (i <- 0 to linkedForwards.length - 2) yield {
  //       val reversed = linkedForwards.reverse
  //       val cell = reversed(i)
  //       val neighbor = reversed(i + 1)
  //       unvisited = unvisited.filter(c => c.coords != cell.coords) 
  //       linker.link(cell, neighbor, bidi=true).head
  //     }
  //     visited = visited ++ linkedBiDirectional
  //     // unvisited = Nil
  //   }
  //   grid.copy(seed = seed).unflatten(visited)
  // }
  


