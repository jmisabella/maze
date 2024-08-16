package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import maze.classes.Neighbors

import java.util.Random // TODO: remove

trait Wilsons extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE

  def generate(grid: Grid): Grid = {
    var nextGrid = grid 
    var unvisited = scala.collection.mutable.Set[Cell]()
    val startCell = nextGrid.get(nextGrid.startCoords.x, nextGrid.startCoords.y)
    
    // Mark all cells as unvisited except for the starting cell
    nextGrid.flatten().foreach(cell => unvisited.add(cell))
    unvisited.remove(startCell)

    val random = new Random()
    
    while (unvisited.nonEmpty) {
      println("MAKING RANDOM WALK ON UNVISITED CELLS")
      val currentCell = unvisited.head
      val path = randomWalk(currentCell, unvisited, nextGrid, random)
      if (path.nonEmpty) {
        var previousCell = path.head
        println("PATH IS NOT EMPTY: " + path.mkString(","))
        path.foreach { cell =>
          if (cell.coords != previousCell.coords) {
            var nextCell = nextGrid.get(cell.coords.x, cell.coords.y)
            println("CHECKING WHETHER CELL IS LINKED")
            if (!previousCell.isLinked(nextCell)) {
              // throw new Exception("LINKING!")
              println("LINKING")
              previousCell = previousCell.copy(linked = previousCell.linked ++  Set(nextCell.coords))
              nextCell = nextCell.copy(linked = nextCell.linked ++ Set(previousCell.coords)) // Link back to the original cell
              nextGrid = nextGrid.set(previousCell)
              nextGrid = nextGrid.set(nextCell)
              println(nextGrid.asci())
              previousCell = nextCell
              if (path.length == 1) {
                var lastCell: Cell = path.head
                nextCell = nextCell.copy(linked = nextCell.linked ++ Set(lastCell.coords))
                lastCell = lastCell.copy(linked = lastCell.linked ++ Set(nextCell.coords))
                nextGrid = nextGrid.set(nextCell)
                nextGrid = nextGrid.set(lastCell)
              }
            }
          }
          // Mark the cell as visited
          val countBeforeRemoval = unvisited.toSeq.length
          println(s"UNVISITED COUNT BEFORE REMOVAL: ${unvisited.toSeq.length}") 
          // unvisited.remove(cell.copy(linked = Set()))
          unvisited.remove(cell)
          // unvisited = unvisited.filter(c => c.coords == cell.coords)
          val countAfterRemoval = unvisited.toSeq.length
          println(s"UNVISITED COUNT AFTER REMOVAL: ${unvisited.toSeq.length}") 
          if (countBeforeRemoval > 0 && countBeforeRemoval == countAfterRemoval) {
            println("COORDS WERE NOT MARKED AS VISITED: " + cell.coords)
          }
        }
      } else {
        println("PATH IS EMPTY, REMOVE ANOTHER CELL FROM UNVISITED")
        // If no path was found, simply remove the current cell from unvisited
        unvisited.remove(currentCell)
      }
    }
    println("RETURNING GRID") 
    nextGrid // Return the modified grid
  }

  private def randomWalk(startCell: Cell, unvisited: scala.collection.mutable.Set[Cell], grid: Grid, random: Random): List[Cell] = {
    println("BEGIN RANDOM WALK")
    var currentCell = startCell
    val path = scala.collection.mutable.ListBuffer[Cell](currentCell)
    val visitedDuringWalk = scala.collection.mutable.Set[Cell](currentCell)

    while (path.forall(cell => cell.linked.isEmpty)) {
      val neighbors = currentCell.availableNeighbors()
      println(s"RANDOM WALK: Current cell: $currentCell, determining unvisited neighbors...")

      val unvisitedNeighbors = neighbors.flatMap { coords =>
        val neighborCell = grid.get(coords.x, coords.y)
        if (unvisited.contains(neighborCell) && !visitedDuringWalk.contains(neighborCell)) {
          println(s"RANDOM WALK: Adding unvisited neighbor $neighborCell")
          Some(neighborCell)
        } else {
          println(s"RANDOM WALK: Neighbor $neighborCell is either visited or not unvisited.")
          None
        }
      }

      if (unvisitedNeighbors.isEmpty) {
        println("RANDOM WALK: No more unvisited neighbors, ending walk.")
        return path.toList
      } else {
        println("RANDOM WALK: Selecting a random unvisited neighbor...")
        currentCell = unvisitedNeighbors(random.nextInt(unvisitedNeighbors.size))
        path += currentCell
        visitedDuringWalk.add(currentCell)
      }
    }
    println("RANDOM WALK: Returning path: " + path.mkString(", "))
    path.toList
  }

}



  // private def randomWalk(startCell: Cell, unvisited: scala.collection.mutable.Set[Cell], grid: Grid, random: Random): List[Cell] = {
  //   var currentCell = startCell
  //   var path = List(currentCell)
    
  //   // Randomly walk until a cell is reached that is already linked
  //   while (!path.exists(cell => cell.linked.nonEmpty)) {
  //     val nextCell = currentCell.availableNeighbors()
  //       .filter(unvisited.map(_.coords).contains())
  //       .drop(random.nextInt(currentCell.availableNeighbors().size))

  //     if (nextCell.isEmpty) {
  //       // If no available neighbors, break the loop
  //       path = List.empty
  //     } else {
  //       currentCell = nextCell.head
  //       path = path :+ currentCell
  //     }
  //   }

  //   path
  // }

// private def randomWalk(startCell: Cell, unvisited: scala.collection.mutable.Set[Cell], grid: Grid, random: Random): List[Cell] = {
//   var currentCell = startCell
//   var path = List(currentCell)
  
//   // Randomly walk until a cell is reached that is already linked
//   while (!path.exists(cell => cell.linked.nonEmpty)) {
//     // Get available neighbors as Coordinates
//     val neighbors = currentCell.availableNeighbors()
    
//     // Filter to find unvisited cells that correspond to the neighbors
//     val unvisitedNeighbors = neighbors.flatMap { coords =>
//       val neighborCell = grid.get(coords.x, coords.y)
//       if (unvisited.contains(neighborCell)) Some(neighborCell) else None
//     }
    
//     if (unvisitedNeighbors.isEmpty) {
//       // If no available neighbors, break the loop
//       return List.empty
//     } else {
//       // Randomly select a neighbor from the unvisited neighbors
//       currentCell = unvisitedNeighbors(random.nextInt(unvisitedNeighbors.size))
//       path = path :+ currentCell
//     }
//   }
//   path
// }

  // override def generate(grid: Grid): Grid = {
  //   var nextGrid = grid
  //   val unvisited = scala.collection.mutable.Set[Cell]()
  //   val startCell = nextGrid.get(nextGrid.startCoords.x, nextGrid.startCoords.y)
    
  //   // Mark all cells as unvisited except for the starting cell
  //   nextGrid.flatten().foreach(cell => unvisited.add(cell))
  //   unvisited.remove(startCell)

  //   // Randomly shuffle unvisited cells
  //   val random = new Random()
    
  //   while (unvisited.nonEmpty) {
  //     val currentCell = unvisited.head
  //     val path = randomWalk(currentCell, unvisited, nextGrid, random)

  //     path.foreach { cell =>
  //       var nextCell = nextGrid.get(cell.coords.x, cell.coords.y)
  //       var mutCell = cell
  //       if (!cell.isLinked(nextCell)) {
  //         mutCell = mutCell.copy(linked = mutCell.linked ++ Set(nextCell.coords))
  //         nextCell = nextCell.copy(linked = nextCell.linked ++ Set(mutCell.coords)) // link back to original cell
  //         nextGrid = nextGrid.set(nextCell)
  //       }
  //       // Mark the cell as visited
  //       unvisited.remove(cell)
  //     }
  //   }
  //   nextGrid // Return the modified grid
  // } 
  
  // private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]], grid: Grid): Seq[Cell] = {
  //   // val unvisitedNeighbors: Seq[Coordinates] = cell.neighbors.toSeq.filter(c => !visited(c.x)(c.y))
  //   // val unvisitedNeighbors: Seq[Coordinates] = cell.neighbors.toSeq.filter(c => !visited(c.y)(c.x))
  //   // val unvisitedNeighbors: Seq[Cell] = cell.neighbors.toSeq.filter(c => !visited(c.y)(c.x)).map(c => cells(c.y)(c.x))
  //   // val unvisitedNeighbors: Seq[Cell] = cell.neighbors.toSeq.filter(c => !visited(c.x)(c.y)).map(c => cells(c.x)(c.y))
  //   val unvisitedNeighbors: Seq[Cell] = cell.neighbors.toSeq.filter(c => !visited(c.y)(c.x)).map(c => grid.cells(c.y)(c.x))
  //   unvisitedNeighbors
  // }
  
  // // private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]], grid: Grid): List[Cell] = {
  // //   // def reverseCoords(coords: Coordinates): Coordinates = Coordinates(coords.y, coords.x) // TODO: remove after the coords reverse bug is fixed
  // //   val neighbors = List(
  // //     (cell.coords.x, cell.coords.y - 1, cell.neighbors.north.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.south.getOrElse(Coordinates(-1, -1)))), // North
  // //     (cell.coords.x, cell.coords.y + 1, cell.neighbors.south.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.north.getOrElse(Coordinates(-1, -1)))), // South
  // //     (cell.coords.x - 1, cell.coords.y, cell.neighbors.west.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.east.getOrElse(Coordinates(-1, -1)))),   // West
  // //     (cell.coords.x + 1, cell.coords.y, cell.neighbors.east.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.west.getOrElse(Coordinates(-1, -1))))   // East
  // //     // (cell.coords.x, cell.coords.y - 1, cell.neighbors.north.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.south.getOrElse(Coordinates(-1, -1))))), // North
  // //     // (cell.coords.x, cell.coords.y + 1, cell.neighbors.south.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.north.getOrElse(Coordinates(-1, -1))))), // South
  // //     // (cell.coords.x - 1, cell.coords.y, cell.neighbors.west.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.east.getOrElse(Coordinates(-1, -1))))),   // West
  // //     // (cell.coords.x + 1, cell.coords.y, cell.neighbors.east.isDefined, (c: Cell) => cell.linked.contains(reverseCoords(c.neighbors.west.getOrElse(Coordinates(-1, -1)))))   // East
  // //   ).collect {
  // //     case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < grid.columns && ny < grid.rows && !visited(nx)(ny) =>
  // //       if (wallOpen) {
  // //         // connect(grid.cells(nx)(ny))
  // //         connect(grid.cells(ny)(nx))
  // //       }
  // //       // grid.cells(nx)(ny)
  // //       grid.cells(ny)(nx)
  // //   }
  // //   println("UNVISITED NEIGHBORS: " + neighbors.mkString(","))
  // //   neighbors
  // // }

  // private def connectCells(path: Seq[Cell], grid: Grid): Grid = {
  //   var nextGrid: Grid = grid
  //   for (i <- 0 until path.length - 1) {
  //     var current = path(i)
  //     var next = path(i + 1)

  //     val linked = linker.link(Seq(current, next))

  //     current = linked.head
  //     next = linked.tail.head
  //     nextGrid = nextGrid.set(current).set(next)
  //     println("LINKED")
  //   }
  //   nextGrid
  // }


  // override def generate(grid: Grid): Grid = {
  //   var nextGrid: Grid = grid
  //   val cells: Array[Array[Cell]] = nextGrid.cells
  //   val random = new Random()
  //   val (width, height): (Int, Int) = (nextGrid.columns, nextGrid.rows)

  //   val visited = Array.fill(width, height)(false)
  //   val stack = scala.collection.mutable.Stack[Cell]()

  //   // Start from a random cell
  //   val startX = random.nextInt(width)
  //   val startY = random.nextInt(height)
  //   // var currentCell = cells(startX)(startY)
  //   var currentCell = cells(startY)(startX)
  //   stack.push(currentCell)
  //   // visited(startX)(startY) = true
  //   visited(startY)(startX) = true

  //   while (stack.nonEmpty) {
  //     // Perform random walks until we find a cell that has already been visited
  //     currentCell = stack.pop()
  //     val path = scala.collection.mutable.ListBuffer[Cell]()
  //     path += currentCell

  //     var break = false
  //     while (!break) {
  //       // Get the neighbors
  //       val neighbors = getUnvisitedNeighbors(currentCell, visited, nextGrid)

  //       if (neighbors.isEmpty) {
  //         // No unvisited neighbors; break the loop
  //         if (path.length > 1) {
  //           nextGrid = connectCells(path.toSeq, nextGrid)
  //           for (cell <- path.toSeq) {
  //             visited(cell.coords.y)(cell.coords.x) = true
  //           }
  //         }
  //         if (stack.isEmpty) {
  //           break = true
  //         }
  //         if (!break) {
  //           currentCell = stack.pop()
  //           path.clear()
  //           path += currentCell
  //         }
  //       } else {
  //         if (!break) {
  //           // Randomly select a neighbor
  //           val nextCell = neighbors(random.nextInt(neighbors.length))
  //           path += nextCell
  //           stack.push(currentCell)
  //           stack.push(nextCell)
  //           currentCell = nextCell
  //           // visited(currentCell.coords.x)(currentCell.coords.y) = true
  //           visited(currentCell.coords.y)(currentCell.coords.x) = true
  //           // TODO: outstanding bug in which cells' coords are reversed
  //           // visited(currentCell.coords.y)(currentCell.coords.x) = true
  //         } 
  //       }
  //     }
  //   }
  //   nextGrid
  // }
// }
