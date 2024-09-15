package maze.behaviors

import maze.behaviors.Cell
import maze.classes.{ SquareGrid, SquareCell, SquareNeighbors, Coordinates, MazeType }
import maze.classes.MazeType._
import maze.utilities.RNG // can control initial seed to ensure repeatability for testing
import scala.util.Random // used to randomly seed our custom RNG for non-testing
import scala.reflect.ClassTag

trait Grid[C <: Cell] {
  
  type NEIGHBORS <: Neighbors

  // type CELL <: Cell

  def mazeType: MazeType


  def width: Int
  def height: Int  
  def cells: Array[Array[C]]
  def seed: RNG
  def startCoords: Coordinates
  def goalCoords: Coordinates
  def asci(): String

  def neighbors(cell: C): Seq[C] = cell.neighbors.toSeq().map(c => get(c.x, c.y))
  
  def neighbors(coords: Coordinates): Seq[C] = get(coords).neighbors.toSeq().map(c => get(c.x, c.y))
  
  def linkOneUnreachable[G <: Grid[C]]()(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = this.asInstanceOf[G]
    if (!nextGrid.isFullyConnected()) {
      var reachableCells: Seq[C] = nextGrid.reachable()
      var unreachableCells: Seq[C] = nextGrid.unreachable()
      for (unreached <- unreachableCells) {
        for (neighborCoords <- unreached.unlinkedNeighbors()) {
          var cell: C = unreached 
          var neighbor: C = get(neighborCoords)
          if (reachable.contains(neighbor)) {
            cell = Cell.setLinked[NEIGHBORS, C](cell, linked = cell.linked ++ Set(neighbor.coords))
            neighbor = Cell.setLinked[NEIGHBORS, C](neighbor, linked = neighbor.linked ++ Set(cell.coords))
            nextGrid = nextGrid.set[G](cell).set[G](neighbor)
            return nextGrid
          }
        } 
      }
    }
    nextGrid
  }
  def linkUnreachables[G <: Grid[C]]()(implicit ct: ClassTag[C]): G = {
    var nextGrid = this
    while (!nextGrid.isFullyConnected()) {
      nextGrid = nextGrid.linkOneUnreachable()
    }
    nextGrid.asInstanceOf[G]
  }

  def allConnectedCells(startCell: C): Seq[C] = {
    var connected: Seq[C] = Seq(startCell)
    var frontier: Seq[C] = Seq(startCell)
    while (!frontier.isEmpty) {
      var newFrontier: Seq[C] = Nil
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
  
  def reachable(): Seq[C] = allConnectedCells(get(startCoords))
  def unreachable(): Seq[C] = flatten().diff(reachable())
  def isFullyConnected(): Boolean = allConnectedCells(get(startCoords)).size == size()
  
  def isPerfectMaze(): Boolean = {
    // each edge is counted twice, so divide by 2
    def countEdges[G <: Grid[C]](grid: G): Int = flatten().map(_.linked.toSeq.length).sum / 2
    // a maze is perfect if it's fully connected and is a tree (no cycles and exactly v-1 edges)
    isFullyConnected() && countEdges(this) == size() - 1
  }

  def flatten(): List[C] = cells.toList.flatten

  // given list of Cells, converts list to grid (array of arrays of cells)
  // prerequisite: provided list's length equals our grid's rows multiplied by columns
  def unflatten[C <: Cell, G <: Grid[C]](flattened: Seq[C])(implicit ct: ClassTag[C]): G = {
    val grouped = flattened.groupBy(c => (c.coords, c.visited, c.neighbors, c.value, c.distance, c.onSolutionPath))
    val merged: Seq[Option[C]] = grouped.foldLeft(Nil: Seq[Option[C]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: NEIGHBORS = k._3.asInstanceOf[NEIGHBORS]
        val value: String = k._4
        val distance: Int = k._5
        val onSolutionPath: Boolean = k._6
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        val cell: C = Cell.instantiate(mazeType, coords, neighbors, linked, distance)
        acc ++ Seq(Some(cell))
      }
    }
    val mergedCells: Seq[C] = merged.filter(_.isDefined).map(_.get)
    // var remaining: List[C] = mergedCells.toList.sorted
    // var remaining: List[C] = flattened.head.sortList[N, C](mergedCells.toList).toList
    var remaining: List[C] = flattened.toList.sortBy(_.coords.inverse())

    Grid.instantiate[NEIGHBORS, C, G](mazeType, height, width, startCoords, goalCoords, remaining)
  }

  def get(x: Int, y: Int): C = cells(y)(x).asInstanceOf[C]
  def get(coords: Coordinates): C = get(coords.x, coords.y)
  def get(cell: C): C = get(cell.coords)
  
  // given a cell (which tracks its own x,y coordinates) updates grid's cell at those coordinates
  def set[G <: Grid[C]](cell: C)(implicit ct: ClassTag[C]): G = {
    val cells: Array[Array[C]] = (for (row <- this.cells) yield {
      (for (c <- row) yield { 
        c.coords match {
          case cell.coords => cell
          case _ => c
        }
      }).toArray
    }).toArray
    Grid.setCells[NEIGHBORS, C, G](this.asInstanceOf[G], cells)
  }

  def size(): Int = cells.length * cells.headOption.getOrElse(Array(0)).length

  def links(cell: C): Seq[C] = (for (c <- cell.linked) yield cells(c.y)(c.x)).toSeq

  def linked(cell1: C, cell2: C): Boolean = cell1.isLinked(cell2)

  // // given a cell, returns its unlinked neighbor cells
  def unlinkedNeighbors(cell: C): Seq[C] = cell.unlinkedNeighbors().map(c => get(c.x, c.y))
  def unlinkedNeighbors(coords: Coordinates): Seq[C] = get(coords).unlinkedNeighbors().map(c => get(c.x, c.y))
  // given a cell, returns its linked neighbor cells
  def linkedNeighbors(cell: C): Seq[C] = cell.linkedNeighbors().map(c => get(c.x, c.y))
  def linkedNeighbors(coords: Coordinates): Seq[C] = get(coords).linkedNeighbors().map(c => get(c.x, c.y))

  def randomInt(upperBoundary: Int): (Int, RNG) = seed.boundedPositiveInt(upperBoundary)
  def randomBoolean(): (Boolean, RNG) = seed.nextBoolean

  def foreach(block: C => Unit): Unit = cells.foreach(row => row.foreach(block))
  def count(p: C => Boolean): Int = flatten().count(p)
  def map[G <: Grid[C]](f: C => C)(implicit ct: ClassTag[C]): G = unflatten[C, G](flatten().map(f))
  // TODO: I think we need to re-work filtering in order to preserve original grid size! 
  // TODO: If cells are filtered out then grid should use null to represent missing cells.
  def withFilter[G <: Grid[C]](p: C => Boolean)(implicit ct: ClassTag[C]): G = unflatten[C, G](flatten().filter(p))
  def filter[G <: Grid[C]](p: C => Boolean)(implicit ct: ClassTag[C]): G = withFilter(p)
  def contains(c: C): Boolean = flatten().contains(c)
  def contains(cs: Seq[C]): Boolean = flatten().foldLeft(false)((acc, c) => flatten().contains(c))
  def find(p: C => Boolean): Option[C] = flatten().find(p)
}

object Grid {
  def instantiate[N <: Neighbors, C <: Cell, G <: Grid[C]](mazeType: MazeType, height: Int, width: Int, startCoords: Coordinates, 
    goalCoords: Coordinates)(implicit ct1: ClassTag[C]): G = {
    
    val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
    
    mazeType match {
      case Square => {
        SquareGrid(height, width, startCoords, goalCoords).asInstanceOf[G]
      }
      case t => throw new IllegalArgumentException("Unexpected MazeType [" + t + "]")
    }
  }
  def instantiate[N <: Neighbors, C <: Cell, G <: Grid[C]](mazeType: MazeType, height: Int, width: Int, startCoords: Coordinates, 
    goalCoords: Coordinates, seed: RNG, flattened: List[C])(implicit ct1: ClassTag[C]): G = {
    
    var remaining: List[C] = flattened
    if (remaining.isEmpty) {
      remaining = Array.ofDim[C](height, width).toList.flatten
    }
    val cells = (for (row <- 0 until height) yield {
      (for (col <- 0 until width) yield {
        var cell = remaining.head
        remaining = remaining.tail
        val coordinates: Coordinates = Coordinates(col, row)
        println("CELL IS NULL? " + cell == null)
        println("CELL COORDS: " + cell.coords)
        Cell.instantiate[N, C](cell, isStart = coordinates == startCoords, isGoal = coordinates == goalCoords)
      }).toArray
    }).toArray
    mazeType match {
      case Square => {
        val grid = SquareGrid(height, width, cells.asInstanceOf[Array[Array[SquareCell]]], seed, startCoords, goalCoords).asInstanceOf[G]
        grid
      }
      case t => throw new IllegalArgumentException("Unexpected MazeType [" + t + "]")
    }
  }
  def instantiate[N <: Neighbors, C <: Cell, G <: Grid[C]](mazeType: MazeType, height: Int, width: Int, startCoords: Coordinates, 
    goalCoords: Coordinates, flattened: List[C] = Nil)(implicit ct1: ClassTag[C]): G = {

      // seed not provide, so randomly generate the initial seed
      val seed: RNG = RNG.RandomSeed(Random.nextInt(height * width + 1))
      instantiate[N, C, G](mazeType, height, width, startCoords, goalCoords, seed, flattened)
  }
  def setCells[N <: Neighbors, C <: Cell, G <: Grid[C]](grid: G, cells: Array[Array[C]])(implicit ct: ClassTag[C]): G = {
    instantiate[N, C, G](grid.mazeType, grid.height, grid.width, grid.startCoords, grid.goalCoords, cells.toList.flatten)
  } 
  def setSeed[N <: Neighbors, C <: Cell, G <: Grid[C]](grid: G, seed: RNG)(implicit ct: ClassTag[C]): G = {
    instantiate[N, C, G](grid.mazeType, grid.height, grid.width, grid.startCoords, grid.goalCoords, seed, grid.flatten)
  } 
}