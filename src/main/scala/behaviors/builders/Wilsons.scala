package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.MazeType._
import maze.behaviors.{ Linkage, Cell, Grid, Neighbors }
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

trait Wilsons[N <: Neighbors, C <: Cell, G <: Grid[C]] extends Generator[N, C, G] {

  private def randomWalk(startCell: C, unvisited: scala.collection.mutable.Set[C], grid: G)(implicit ct: ClassTag[C]): (List[C], G) = {
    var nextGrid = grid
    var currentCell = startCell
    val path = scala.collection.mutable.ListBuffer[C](currentCell)
    val visitedDuringWalk = scala.collection.mutable.Set[C](currentCell)
    while (true) {
      val neighbors: Seq[Coordinates] = currentCell.neighborCoords()
      val unvisitedNeighbors: Seq[C] = neighbors.flatMap { coords =>
        val neighborCell = grid.get(coords.x, coords.y)
        if (unvisited.contains(neighborCell) && !visitedDuringWalk.contains(neighborCell)) {
          Some(neighborCell)
        } else {
          None
        }
      }
      if (unvisitedNeighbors.isEmpty) {
        return (path.toList, nextGrid)
      } else {
        val (randomIndex, seed): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = nextGrid.set[N, C, G](seed = seed)
        currentCell = unvisitedNeighbors(randomIndex)
        path += currentCell
        visitedDuringWalk.add(currentCell)
      }
    }
    (path.toList, nextGrid)
  }

  def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid = grid 
    var unvisited = scala.collection.mutable.Set[C]()
    val startCell = nextGrid.get(nextGrid.startCoords.x, nextGrid.startCoords.y)
    // Mark all cells as unvisited except for the starting cell
    nextGrid.flatten().foreach(cell => unvisited.add(cell))
    unvisited.remove(startCell)
    while (unvisited.nonEmpty) {
      val currentCell = unvisited.head
      val (path, updatedGrid) = randomWalk(currentCell, unvisited, nextGrid)
      nextGrid = updatedGrid
      if (path.nonEmpty) {
        var previousCell = path.head
        path.foreach { cell =>
          if (cell.coords != previousCell.coords) {
            var nextCell = nextGrid.get(cell.coords.x, cell.coords.y)
            if (!previousCell.isLinked(nextCell)) {
              previousCell = previousCell.setLinked(linked = previousCell.linked ++  Set(nextCell.coords))
              nextCell = nextCell.setLinked(linked = nextCell.linked ++ Set(previousCell.coords)) // Link back to the original cell
              nextGrid = nextGrid.set[G](previousCell).set(nextCell)
              //// why does the below line yield different results than when linking without linker??
              // nextGrid = linker.link(previousCell, nextCell, nextGrid)
              previousCell = nextCell
              if (path.length == 1) {
                var lastCell: C = path.head
                nextCell = nextCell.setLinked(linked = nextCell.linked ++ Set(lastCell.coords))
                lastCell = lastCell.setLinked(linked = lastCell.linked ++ Set(nextCell.coords))
                nextGrid = nextGrid.set[G](nextCell).set(lastCell)
                //// why does the below line yield different results than when linking without linker??
                // nextGrid = linker.link(nextCell, lastCell, nextGrid)
              }
            }
          }
          // Mark the cell as visited
          unvisited.remove(cell)
        }
      }
    }
    nextGrid.linkUnreachables() // return modified grid
  }


}

