package behaviors.builders.jmi

import maze.classes.{ Cell, Coordinates, Grid }
import maze.behaviors.Linkage

// TODO: this implementation seems to work for Wilsons using my Cells class

import scala.util.Random

// case class Cell(x: Int, y: Int) {
//   var north: Boolean = true
//   var south: Boolean = true
//   var east: Boolean = true
//   var west: Boolean = true
  
//   override def toString: String = {
//     s"Cell($x, $y, N=$north, S=$south, E=$east, W=$west)"
//   }
// }

class Maze2(val width: Int, val height: Int) {

  case object linker extends Linkage

  private val cells: Array[Array[Cell]] = Array.tabulate(width, height) { (x, y) => Cell(x, y) }
  private val random = new Random()

  def generateMaze(): Unit = {
    val visited = Array.fill(width, height)(false)
    val stack = scala.collection.mutable.Stack[Cell]()

    // Start from a random cell
    val startX = random.nextInt(width)
    val startY = random.nextInt(height)
    // var currentCell = cells(startX)(startY)
    var currentCell = cells(startY)(startX)
    stack.push(currentCell)
    visited(startX)(startY) = true

    while (stack.nonEmpty) {
      // Perform random walks until we find a cell that has already been visited
      currentCell = stack.pop()
      val path = scala.collection.mutable.ListBuffer[Cell]()
      path += currentCell

      var break = false
      while (!break) {
        // Get the neighbors
        val neighbors = getUnvisitedNeighbors(currentCell, visited)

        if (neighbors.isEmpty) {
          // No unvisited neighbors; break the loop
          if (path.length > 1) {
            connectCells(path.toSeq)
          }
          if (stack.isEmpty) {
            break = true
          }
          if (!break) {
            currentCell = stack.pop()
            path.clear()
            path += currentCell
          }
        } else {
          if (!break) {
            // Randomly select a neighbor
            val nextCell = neighbors(random.nextInt(neighbors.length))
            path += nextCell
            stack.push(currentCell)
            currentCell = nextCell
            // visited(currentCell.coords.x)(currentCell.coords.y) = true
            // TODO: outstanding bug in which cells' coords are reversed
            visited(currentCell.coords.y)(currentCell.coords.x) = true
          } 
        }
      }
    }
  }

  // private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]]): Seq[Cell] = {
  //   // val unvisitedNeighbors: Seq[Coordinates] = cell.neighbors.toSeq.filter(c => !visited(c.x)(c.y))
  //   // val unvisitedNeighbors: Seq[Coordinates] = cell.neighbors.toSeq.filter(c => !visited(c.y)(c.x))
  //   // val unvisitedNeighbors: Seq[Cell] = cell.neighbors.toSeq.filter(c => !visited(c.y)(c.x)).map(c => cells(c.y)(c.x))
  //   // val unvisitedNeighbors: Seq[Cell] = cell.neighbors.toSeq.filter(c => !visited(c.x)(c.y)).map(c => cells(c.x)(c.y))
  //   val unvisitedNeighbors: Seq[Cell] = cell.neighbors.toSeq.filter(c => !visited(c.y)(c.x)).map(c => cells(c.y)(c.x))
  //   unvisitedNeighbors
  // }


  // private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]]): List[Cell] = {
  //   println("CHECKING COORDS " + cell)
  //   // val north: Cell = if (cell.neighbors.north.isDefined) cells(cell.coords.x)(cell.coords.y - 1) else Cell(-1, -1)
  //   // val south: Cell = if (cell.neighbors.south.isDefined) cells(cell.coords.x)(cell.coords.y + 1) else Cell(-1, -1)
  //   // val west: Cell = if (cell.neighbors.west.isDefined) cells(cell.coords.x - 1)(cell.coords.y) else Cell(-1, -1)
  //   // val east: Cell = if (cell.neighbors.east.isDefined) cells(cell.coords.x + 1)(cell.coords.y) else Cell(-1, -1)
  //   val north: Cell = if (cell.neighbors.north.isDefined) cells(cell.coords.y)(cell.coords.x - 1) else Cell(-1, -1)
  //   val south: Cell = if (cell.neighbors.south.isDefined) cells(cell.coords.y)(cell.coords.x + 1) else Cell(-1, -1)
  //   val west: Cell = if (cell.neighbors.west.isDefined) cells(cell.coords.y - 1)(cell.coords.x) else Cell(-1, -1)
  //   val east: Cell = if (cell.neighbors.east.isDefined) cells(cell.coords.y + 1)(cell.coords.x) else Cell(-1, -1)
  //   // TODO: need to change function (connect, the 4th returned arg)  
  //   val neighbors = List(
  //     // // (north.coords.x, north.coords.y, cell.neighbors.north.isDefined && !cell.linked.contains(north.coords), (c: Cell) => cells(south.coords.x)(south.coords.y)), // North
  //     // // (south.coords.x, south.coords.y, cell.neighbors.south.isDefined && !cell.linked.contains(south.coords), (c: Cell) => cells(north.coords.x)(north.coords.y)), // South
  //     // // (west.coords.x, west.coords.y, cell.neighbors.west.isDefined && !cell.linked.contains(west.coords), (c: Cell) => cells(east.coords.x)(east.coords.y)),   // West
  //     // // (east.coords.x, west.coords.y, cell.neighbors.east.isDefined && !cell.linked.contains(east.coords), (c: Cell) => cells(west.coords.x)(west.coords.y))    // East
  //     // (north.coords.y, north.coords.x, cell.neighbors.north.isDefined && !cell.linked.contains(north.coords), (c: Cell) => cells(south.coords.y)(south.coords.x)), // North
  //     // (south.coords.y, south.coords.x, cell.neighbors.south.isDefined && !cell.linked.contains(south.coords), (c: Cell) => cells(north.coords.y)(north.coords.x)), // South
  //     // (west.coords.y, west.coords.x, cell.neighbors.west.isDefined && !cell.linked.contains(west.coords), (c: Cell) => cells(east.coords.y)(east.coords.x)),   // West
  //     // (east.coords.y, west.coords.x, cell.neighbors.east.isDefined && !cell.linked.contains(east.coords), (c: Cell) => cells(west.coords.y)(west.coords.x))    // East
  //     (north.coords.y, north.coords.x, cell.neighbors.north.isDefined && !cell.linked.contains(north.coords), (c: Cell) => linker.link(Seq(c, cells(south.coords.y)(south.coords.x)))), // North
  //     (south.coords.y, south.coords.x, cell.neighbors.south.isDefined && !cell.linked.contains(south.coords), (c: Cell) => linker.link(Seq(c, cells(north.coords.y)(north.coords.x)))), // South
  //     (west.coords.y, west.coords.x, cell.neighbors.west.isDefined && !cell.linked.contains(west.coords), (c: Cell) => linker.link(Seq(c, cells(east.coords.y)(east.coords.x)))),   // West
  //     (east.coords.y, west.coords.x, cell.neighbors.east.isDefined && !cell.linked.contains(east.coords), (c: Cell) => linker.link(Seq(c, cells(west.coords.y)(west.coords.x))))    // East
  //   ).collect {
  //     case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(nx)(ny) =>
  //       if (wallOpen) {
  //         connect(cells(nx)(ny))
  //       }
  //       cells(nx)(ny)
  //   }
  //   println("UNVISITED NEIGHBORS: " + neighbors.mkString(","))
  //   neighbors
  // }
  
  private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]]): List[Cell] = {
    val neighbors = List(
      // (cell.coords.x, cell.coords.y - 1, cell.neighbors.north.isDefined, (c: Cell) => c.neighbors.south.isDefined), // North
      // (cell.coords.x, cell.coords.y + 1, cell.neighbors.south.isDefined, (c: Cell) => c.neighbors.north.isDefined), // South
      // (cell.coords.x - 1, cell.coords.y, cell.neighbors.west.isDefined, (c: Cell) => c.neighbors.east.isDefined),   // West
      // (cell.coords.x + 1, cell.coords.y, cell.neighbors.east.isDefined, (c: Cell) => c.neighbors.west.isDefined)    // East
      (cell.coords.x, cell.coords.y - 1, cell.neighbors.north.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.south.get)), // North
      (cell.coords.x, cell.coords.y + 1, cell.neighbors.south.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.north.get)), // South
      (cell.coords.x - 1, cell.coords.y, cell.neighbors.west.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.east.get)),   // West
      (cell.coords.x + 1, cell.coords.y, cell.neighbors.east.isDefined, (c: Cell) => cell.linked.contains(c.neighbors.west.get))   // East
    ).collect {
      case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(nx)(ny) =>
        if (wallOpen) {
          // connect(cells(nx)(ny))
          connect(cells(ny)(nx))
        }
        // cells(nx)(ny)
        cells(ny)(nx)
    }
    println("UNVISITED NEIGHBORS: " + neighbors.mkString(","))
    neighbors
  }
  
  // private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]]): List[Cell] = {
  //   val neighbors = List(
  //     (cell.x, cell.y - 1, cell.north, (c: Cell) => c.south), // North
  //     (cell.x, cell.y + 1, cell.south, (c: Cell) => c.north), // South
  //     (cell.x - 1, cell.y, cell.west, (c: Cell) => c.east),   // West
  //     (cell.x + 1, cell.y, cell.east, (c: Cell) => c.west)    // East
  //   ).collect {
  //     case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(nx)(ny) =>
  //       if (wallOpen) {
  //         connect(cells(nx)(ny))
  //       }
  //       cells(nx)(ny)
  //   }
  //   println("UNVISITED NEIGHBORS: " + neighbors.mkString(","))
  //   neighbors
  // }

  private def connectCells(path: Seq[Cell]): Unit = {
    for (i <- 0 until path.length - 1) {
      var current = path(i)
      var next = path(i + 1)

      // Remove walls between current and next cell
      val linked = linker.link(Seq(current, next))
      println("LINKED: " + linked)
      current = linked.head.copy(linked = current.linked ++ Set(next.coords)) 
      next = linked.tail.head.copy(linked = next.linked ++ Set(current.coords))
      // current = linked.head.copy(linked = current.linked ++ Set(Coordinates(next.coords.y, next.coords.x))) 
      // next = linked.tail.head.copy(linked = next.linked ++ Set(Coordinates(current.coords.y, next.coords.x)))
      println("LINKED")
      println(Set(next.coords))
      println(Set(current.coords))
      println(current.copy(linked = Set(next.coords)).linked)
      println(next.copy(linked = Set(current.coords)).linked)
      println(cells(current.coords.y)(current.coords.x))
      println(cells(next.coords.y)(next.coords.x))
      // println(cells(current.coords.x)(current.coords.y))
      // println(cells(next.coords.x)(next.coords.y))
      // if (current.coords.x == next.coords.x) {
      //   if (current.coords.y < next.coords.y) {
      //     // current.south = false
      //     // next.north = false
      //     current.south = false
      //     next.north = false
      //   } else {
      //     // current.north = false
      //     // next.south = false
      //     current.north = false
      //     next.south = false
      //   }
      // } else if (current.coords.y == next.coords.y) {
      //   if (current.coords.x < next.coords.x) {
      //     current.east = false
      //     next.west = false
      //   } else {
      //     current.west = false
      //     next.east = false
      //   }
      // }
    }
  }

  // def asci(): String = {
  //   var output: String = "+" + "---+" * width + "\n"
  //   for (row <- cells) {
  //     var top: String = "|"
  //     var bottom: String = "+"
  //     for (cell <- row) {
  //       // val body = " X "
  //       val body = cell.value
  //       val eastBoundary: String = cell.neighbors.east match {
  //         case Some(east) if (cell.isLinked(east)) => " "
  //         case _ => "|"
  //       }
  //       top += body + eastBoundary
  //       val southBoundary: String = cell.neighbors.south match {
  //         case Some(south) if (cell.isLinked(south)) => "   "
  //         case _ => "---"
  //       }
  //       val corner: String= "+"
  //       bottom += southBoundary + corner
  //     }
  //     output += top + "\n"
  //     output += bottom + "\n"
  //   }
  //   output 
  // }

  def asci(): String = {
    // initialize output to the northern wall 
    var output: String = "+" + "---+" * width + "\n"
    for (y <- 0 until height) {
      var top: String = "|"
      var bottom: String = "+"
      for (x <- 0 until width) {
        val cell = cells(x)(y)
        val body = "   "
        // val eastBoundary: String = (cell.east, cell.x) match {
        //   case (true, x) if (x == width - 1)  => "|"
        //   // case (true, _)  => " "
        //   // case (false, _) => "|"
        //   case (true, _)  => "|"
        //   case (false, _) => " "
        // }
        // top += body + eastBoundary
        // val southBoundary: String = (cell.south, cell.y) match {
        //   case (true, y) if (y == height - 1) => "---"
        //   // case (true, _) => "   "
        //   // case (false, _) => "---"
        //   case (true, _) => "---"
        //   case (false, _) => "   "
        // }
        val corner: String= "+"
        val eastBoundary: String = cell.neighbors.east match {
          case Some(east) if (cell.isLinked(east)) => " "
          case _ => "|"
        }
        top += body + eastBoundary
        val southBoundary: String = cell.neighbors.south match {
          case Some(south) if (cell.isLinked(south)) => "   "
          case _ => "---"
        }
        bottom += southBoundary + corner
      }
      output += top + "\n"
      output += bottom + "\n"
    }
    output 
  }

}

object WilsonsMaze2 {
  def run(): Unit = {
    val maze = new Maze2(10, 10)
    maze.generateMaze()
    // maze.printMaze()
    println(maze.asci())
  }

  def main(args: Array[String]): Unit = {
    val maze = new Maze2(10, 10)
    maze.generateMaze()
    // maze.printMaze()
    println(maze.asci())
  }
}

