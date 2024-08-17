package maze.behaviors.builders.chatgpt2

import maze.classes.{ Cell, Grid }

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

class Maze(val width: Int, val height: Int) {
  // private val cells: Array[Array[Cell]] = Array.tabulate(width, height) { (x, y) => Cell(x, y) }
  val cells: Array[Array[Cell]] = Array.tabulate(height, width) { (y, x) => Cell(x, y) }
  private val random = new Random()

  def generateMaze(): Unit = {
    val visited = Array.fill(width, height)(false)
    val stack = scala.collection.mutable.Stack[Cell]()

    // Start from a random cell
    val startX = random.nextInt(width)
    val startY = random.nextInt(height)
    var currentCell = cells(startX)(startY)
    stack.push(currentCell)
    // visited(startX)(startY) = true
    visited(startY)(startX) = true

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
            // visited(currentCell.x)(currentCell.y) = true
            visited(currentCell.coords.y)(currentCell.coords.x) = true
          } 
        }
      }
    }
  }
  
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
  //     (cell.coords.x, cell.coords.y - 1, cell.north, (c: Cell) => c.south), // North
  //     (cell.coords.x, cell.coords.y + 1, cell.south, (c: Cell) => c.north), // South
  //     (cell.coords.x - 1, cell.coords.y, cell.west, (c: Cell) => c.east),   // West
  //     (cell.coords.x + 1, cell.coords.y, cell.east, (c: Cell) => c.west)    // East
  //   ).collect {
  //     // case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(nx)(ny) =>
  //     //   if (wallOpen) {
  //     //     connect(cells(nx)(ny))
  //     //   }
  //     //   cells(nx)(ny)
  //     case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(ny)(nx) =>
  //       if (wallOpen) {
  //         connect(cells(ny)(nx))
  //       }
  //       cells(ny)(nx)
  //   }
  //   neighbors
  // }

  private def connectCells(path: Seq[Cell]): Seq[Cell] = {
    var updatedPath: Seq[Cell] = Seq()
    var previousCell = path.head
    for (i <- 0 until path.length - 1) {
      var current: Cell = path(i)
      var next: Cell = path(i + 1)
      current = current.copy(linked = current.linked ++ Set(next.coords))
      next = next.copy(linked = next.linked ++ Set(current.coords))
      updatedPath = updatedPath.filter(c => c.coords != current.coords && c.coords != next.coords) ++ Seq(current, next)
    }
    updatedPath
  }
  
  def asci(): String = {
    var output: String = "+" + "---+" * width + "\n"
    for (row <- cells) {
      var top: String = "|"
      var bottom: String = "+"
      for (cell <- row) {
        // val body = " X "
        val body = cell.value
        val eastBoundary: String = cell.neighbors.east match {
          case Some(east) if (cell.isLinked(east)) => " "
          case _ => "|"
        }
        top += body + eastBoundary
        val southBoundary: String = cell.neighbors.south match {
          case Some(south) if (cell.isLinked(south)) => "   "
          case _ => "---"
        }
        val corner: String= "+"
        bottom += southBoundary + corner
      }
      output += top + "\n"
      output += bottom + "\n"
    }
    output 
  }

}

object WilsonsMaze {
  def run(width: Int = 4, height: Int = 4): Unit = {
    val maze = new Maze(width, height)
    maze.generateMaze()
    println(maze.asci())
  }

  def main(args: Array[String]): Unit = {
    val maze = new Maze(10, 10)
    maze.generateMaze()
    println(maze.asci())
  }
}


