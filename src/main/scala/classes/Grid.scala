package maze.classes

// import maze.behaviors.Cell
import maze.classes.{ Cell, Coordinates, MazeType }
// import maze.classes.cell.{ SquareCell, TriangleCell, HexCell }
import maze.classes.cell.TriangleOrientation._
// import maze.classes.grid.{ SquareGrid, TriangleGrid, HexGrid }
import maze.classes.MazeType._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing

case class Grid(
  mazeType: MazeType,
  height: Int, 
  width: Int, 
  cells: Array[Array[Cell]],
  seed: RNG,
  startCoords: Coordinates,
  goalCoords: Coordinates) {

  // retrieve row
  def row(y: Int): List[Cell] = cells(y).toList
  // retrieve column
  def column(x: Int): List[Cell] = (for (y <- 0 until height) yield cells(y)(x)).toList

  def neighbors(cell: Cell): Seq[Cell] = cell.neighbors().map(c => get(c.x, c.y))
  
  def neighbors(coords: Coordinates): Seq[Cell] = get(coords).neighbors().map(c => get(c.x, c.y))
  
  def linkOneUnreachable(): Grid = {
    var nextGrid: Grid = this
    if (!nextGrid.isFullyConnected()) {
      var reachableCells: Seq[Cell] = nextGrid.reachable()
      var unreachableCells: Seq[Cell] = nextGrid.unreachable()
      for (unreached <- unreachableCells) {
        for (neighborCoords <- unreached.unlinkedNeighbors()) {
          var cell: Cell = unreached 
          var neighbor: Cell = get(neighborCoords)
          if (reachable.contains(neighbor)) {
            cell = cell.setLinked(linked = cell.linked ++ Set(neighbor.coords))
            neighbor = neighbor.setLinked(linked = neighbor.linked ++ Set(cell.coords))
            nextGrid = nextGrid.set(cell).set(neighbor)
            return nextGrid
          }
        } 
      }
    }
    nextGrid
  }
  def linkUnreachables(): Grid = {
    var nextGrid = this
    while (!nextGrid.isFullyConnected()) {
      nextGrid = nextGrid.linkOneUnreachable()
    }
    nextGrid
  }

  def allConnectedCells(startCell: Cell): Seq[Cell] = {
    var connected: Seq[Cell] = Seq(startCell)
    var frontier: Seq[Cell] = Seq(startCell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[Cell] = Nil
      for (c <- frontier) {
        for (linked <- c.linked) {
          if (!connected.contains(get(linked))) {
            connected = connected ++ Seq(get(linked))
            newFrontier = newFrontier ++ Seq(cells(linked.y)(linked.x))
          }
        }
      }
      frontier = newFrontier
    }
    connected
  }
  
  def reachable(): Seq[Cell] = allConnectedCells(get(startCoords))
  def unreachable(): Seq[Cell] = flatten().diff(reachable())
  def isFullyConnected(): Boolean = allConnectedCells(get(startCoords)).size == size()
  
  def isPerfectMaze(): Boolean = {
    // each edge is counted twice, so divide by 2
    def countEdges(): Int = flatten().map(_.linked.toSeq.length).sum / 2
    // a maze is perfect if it's fully connected and is a tree (no cycles and exactly v-1 edges)
    isFullyConnected() && countEdges() == size() - 1
  }
  //// TODO: could we have a method to bi-directionally link cells which are not already?

  def flatten(): List[Cell] = cells.toList.flatten

  // given list of Cells, converts list to grid (array of arrays of cells)
  // prerequisite: provided list's length equals our grid's rows multiplied by columns
  def unflatten(flattened: Seq[Cell]): Grid = {
    val grouped = flattened.groupBy(c => (c.coords, c.visited, c.neighborsByDirection, c.value, c.distance, c.onSolutionPath))
    val merged: Seq[Option[Cell]] = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighborsByDirection: Map[String, Coordinates ]= k._3
        val value: String = k._4
        val distance: Int = k._5
        val onSolutionPath: Boolean = k._6
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        val cell: Cell = Cell(coords, mazeType, neighborsByDirection, linked, distance)
        acc ++ Seq(Some(cell))
      }
    }
    val mergedCells: Seq[Cell] = merged.filter(_.isDefined).map(_.get)
    var remaining: List[Cell] = flattened.toList.sortBy(_.coords.inverse())

    Grid(mazeType, height, width, startCoords, goalCoords, remaining)
  }

  def get(x: Int, y: Int): Cell = cells(y)(x)
  def get(coords: Coordinates): Cell = get(coords.x, coords.y)
  def get(cell: Cell): Cell = get(cell.coords)
  
  // given a cell (which tracks its own x,y coordinates) updates grid's cell at those coordinates
  def set(cell: Cell): Grid = {
    val cells: Array[Array[Cell]] = (for (row <- this.cells) yield {
      (for (c <- row) yield { 
        c.coords match {
          case cell.coords => cell
          case _ => c
        }
      }).toArray
    }).toArray
    copy(cells = cells)
  }
  def set(cells: Seq[Cell]): Grid = {
    var grid = this
    cells.map(c => grid = grid.set(c))
    grid
  }
  // set RNG seed 
  def set(seed: RNG): Grid = {
    copy(seed = seed)
  } 
  
  def size(): Int = cells.length * cells.headOption.getOrElse(Array(0)).length

  def links(cell: Cell): Seq[Cell] = (for (c <- cell.linked) yield cells(c.y)(c.x)).toSeq

  def linked(cell1: Cell, cell2: Cell): Boolean = cell1.isLinked(cell2)

  // // given a cell, returns its unlinked neighbor cells
  def unlinkedNeighbors(cell: Cell): Seq[Cell] = cell.unlinkedNeighbors().map(c => get(c.x, c.y))
  def unlinkedNeighbors(coords: Coordinates): Seq[Cell] = get(coords).unlinkedNeighbors().map(c => get(c.x, c.y))
  // given a cell, returns its linked neighbor cells
  def linkedNeighbors(cell: Cell): Seq[Cell] = cell.linkedNeighbors().map(c => get(c.x, c.y))
  def linkedNeighbors(coords: Coordinates): Seq[Cell] = get(coords).linkedNeighbors().map(c => get(c.x, c.y))

  def randomInt(upperBoundary: Int): (Int, RNG) = seed.boundedPositiveInt(upperBoundary)
  def randomInt(collection: Seq[Any]): (Int, RNG) = seed.boundedPositiveInt(collection.length)
  def randomBoolean(): (Boolean, RNG) = seed.nextBoolean

  def foreach(block: Cell => Unit): Unit = cells.foreach(row => row.foreach(block))
  def count(p: Cell => Boolean): Int = flatten().count(p)
  def map(f: Cell => Cell): Grid = unflatten(flatten().map(f))
  // TODO: I think we need to re-work filtering in order to preserve original grid size! 
  // TODO: If cells are filtered out then grid should use null to represent missing cells.
  def withFilter(p: Cell => Boolean): Grid = unflatten(flatten().filter(p))
  def filter(p: Cell => Boolean): Grid = withFilter(p)
  def contains(c: Cell): Boolean = flatten().contains(c)
  def contains(cs: Seq[Cell]): Boolean = flatten().foldLeft(false)((acc, c) => flatten().contains(c))
  def find(p: Cell => Boolean): Option[Cell] = flatten().find(p)

  def asci(): String = ""
  
  override def toString(): String = {
    var output: String = "{\"rows\":["
    var currRow: String = ""
    for (row <- cells) {
      if (currRow.length() > 0) {
        output += ","
      } 
      currRow = row.mkString("[", ",", "]")
      output += currRow
    }
    output += "]}"
    output
  }
}

object Grid {
  def apply(mazeType: MazeType, height: Int, width: Int, startCoords: Coordinates, 
    goalCoords: Coordinates): Grid = {
    
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))

    Grid(mazeType, height, width, ???, seed, startCoords, goalCoords)

//   mazeType: MazeType,
//   height: Int, 
//   width: Int, 
//   cells: Array[Array[Cell]],
//   seed: RNG,
//   startCoords: Coordinates,
//   goalCoords: Coordinates) {
  }
  def apply(mazeType: MazeType, height: Int, width: Int, startCoords: Coordinates, 
    goalCoords: Coordinates, seed: RNG, flattened: List[Cell]): Grid = {
    
    var remaining: List[Cell] = flattened
    if (remaining.isEmpty) {
      remaining = Array.ofDim[Cell](height, width).toList.flatten
    }
    val cells = (for (row <- 0 until height) yield {
      (for (col <- 0 until width) yield {
        var cell = remaining.head
        remaining = remaining.tail
        val coordinates: Coordinates = Coordinates(col, row)
        Cell(cell, isStart = coordinates == startCoords, isGoal = coordinates == goalCoords)
      }).toArray
    }).toArray
    Grid(mazeType, height, width, cells, seed, startCoords, goalCoords)
  }
  def apply(mazeType: MazeType, height: Int, width: Int, startCoords: Coordinates, 
    goalCoords: Coordinates, flattened: List[Cell] = Nil): Grid = {
      // seed not provided, so randomly generate the initial seed
      val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
      Grid(mazeType, height, width, startCoords, goalCoords, seed, flattened)
  }
//   private def setCells[C <: Cell, G <: Grid[C]](grid: G, cells: Array[Array[C]])(implicit ct: ClassTag[C]): G = {
//     instantiate[C, G](grid.mazeType, grid.height, grid.width, grid.startCoords, grid.goalCoords, cells.toList.flatten)
//   }
//   private def setCells[C <: Cell, G <: Grid[C]](grid: G, cells: Seq[Seq[C]])(implicit ct: ClassTag[C]): G = {
//     setCells(grid, cells.map(xs => xs.toArray).toArray)
//   }
//   private def setSeed[C <: Cell, G <: Grid[C]](grid: G, seed: RNG)(implicit ct: ClassTag[C]): G = {
//     instantiate[C, G](grid.mazeType, grid.height, grid.width, grid.startCoords, grid.goalCoords, seed, grid.flatten)
//   } 
}
