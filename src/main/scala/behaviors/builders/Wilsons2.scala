package maze.behaviors.builders.chatgpt

import scala.util.Random

case class Cell(x: Int, y: Int) {
  var north: Boolean = true
  var south: Boolean = true
  var east: Boolean = true
  var west: Boolean = true
  
  override def toString: String = {
    s"Cell($x, $y, N=$north, S=$south, E=$east, W=$west)"
  }
}

class Maze(val width: Int, val height: Int) {
  private val cells: Array[Array[Cell]] = Array.tabulate(width, height) { (x, y) => Cell(x, y) }
  private val random = new Random()

  def generateMaze(): Unit = {
    val visited = Array.fill(width, height)(false)
    val stack = scala.collection.mutable.Stack[Cell]()

    // Start from a random cell
    val startX = random.nextInt(width)
    val startY = random.nextInt(height)
    var currentCell = cells(startX)(startY)
    stack.push(currentCell)
    visited(startX)(startY) = true
    // visited(startY)(startX) = true

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
            visited(currentCell.x)(currentCell.y) = true
            // visited(currentCell.y)(currentCell.x) = true
          } 
        }
      }
    }
  }

  private def getUnvisitedNeighbors(cell: Cell, visited: Array[Array[Boolean]]): List[Cell] = {
    val neighbors = List(
      (cell.x, cell.y - 1, cell.north, (c: Cell) => c.south), // North
      (cell.x, cell.y + 1, cell.south, (c: Cell) => c.north), // South
      (cell.x - 1, cell.y, cell.west, (c: Cell) => c.east),   // West
      (cell.x + 1, cell.y, cell.east, (c: Cell) => c.west)    // East
    ).collect {
      case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(nx)(ny) =>
        if (wallOpen) {
          connect(cells(nx)(ny))
        }
        cells(nx)(ny)
      // case (nx, ny, wallOpen, connect) if nx >= 0 && ny >= 0 && nx < width && ny < height && !visited(ny)(nx) =>
      //   if (wallOpen) {
      //     connect(cells(ny)(nx))
      //   }
      //   cells(ny)(nx)
    }
    neighbors
  }

  private def connectCells(path: Seq[Cell]): Unit = {
    for (i <- 0 until path.length - 1) {
      val current = path(i)
      val next = path(i + 1)

      // Remove walls between current and next cell
      if (current.x == next.x) {
        if (current.y < next.y) {
          current.south = false
          next.north = false
        } else {
          current.north = false
          next.south = false
        }
      } else if (current.y == next.y) {
        if (current.x < next.x) {
          current.east = false
          next.west = false
        } else {
          current.west = false
          next.east = false
        }
      }
    }
  }

  def printMaze(): Unit = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        // Print the north wall
        if (cells(x)(y).north) print("+---") else print("+   ")
        // if (cells(y)(x).north) print("+---") else print("+   ")
      }
      println("+")
      for (x <- 0 until width) {
        // Print the west wall and the cell itself
        // if (cells(x)(y).west) print("|") else print("   ")
        // print("   ")
        if (cells(x)(y).west) print("|") else print("   ")
        // if (cells(y)(x).west) print("|") else print("   ")
        print("  ")
      }
      println("|")
    }
    // Print the bottom wall
    for (x <- 0 until width) {
      print("+---")
    }
    println("+")
  }
}

object WilsonsMaze {
  def run(width: Int = 4, height: Int = 8): Unit = {
    val maze = new Maze(width, height)
    maze.generateMaze()
    maze.printMaze()
  }

  def main(args: Array[String]): Unit = {
    val maze = new Maze(10, 10)
    maze.generateMaze()
    maze.printMaze()
  }
}


