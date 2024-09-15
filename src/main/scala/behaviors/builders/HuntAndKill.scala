package maze.behaviors.builders

import maze.classes.{ Coordinates }
import maze.classes.MazeType._
import maze.behaviors.{ Linkage, Cell, Grid, Neighbors }

import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

trait HuntAndKill[N <: Neighbors, C <: Cell, G <: Grid[C]] extends Generator[N, C, G] {
  type LINKAGE <: Linkage[N, C, G]
  val linker: LINKAGE
  
  override def generate(grid: G)(implicit ct: ClassTag[C]): G = {
    var nextGrid: G = grid
    var visited: Seq[Coordinates] = Nil
    val (randomIndex1, seed1): (Int, RNG) = nextGrid.randomInt(nextGrid.size())
    nextGrid = Grid.setSeed[N, C, G](grid = nextGrid, seed = seed1)
    var current: Option[C] = Some(nextGrid.flatten()(randomIndex1))
    while (current != None) {
      val unvisitedNeighbors: Seq[C] = nextGrid.unlinkedNeighbors(current.get).filter(c => !visited.contains(c.coords))
      if (unvisitedNeighbors.nonEmpty) {
        // ****** KILL ****** 
        val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = Grid.setSeed[N, C, G](grid = nextGrid, seed = seed2)
        var neighbor: C = unvisitedNeighbors(randomIndex2)
        // current = Some(Cell.setLinked[N, C](cell = current.get, linked = current.get.linked ++ Set(neighbor.coords)))
        // neighbor = Cell.setLinked[N, C](cell = neighbor, linked = neighbor.linked ++ Set(current.get.coords))
        current = Some(current.get.setLinked[N, C](linked = current.get.linked ++ Set(neighbor.coords)))
        neighbor = neighbor.setLinked[N, C](linked = neighbor.linked ++ Set(current.get.coords))
        nextGrid = nextGrid.set[G](current.get).set(neighbor)
        visited = visited ++ Seq(current.get.coords) ++ Seq(neighbor.coords)
        current = Some(neighbor)
      } else {
        // ****** HUNT ****** 
        current = nextGrid.find(c => { 
          !visited.contains(c.coords) && 
          nextGrid.unlinkedNeighbors(c).count(c2 => !nextGrid.get(c2).linked.isEmpty && visited.contains(c2.coords)) > 0
        })
        if (current.isDefined) {
          val neighborsAlreadyVisited: Seq[C] = nextGrid.unlinkedNeighbors(current.get).filter(c => visited.contains(c.coords))
          if (neighborsAlreadyVisited.length > 0) {
            val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(neighborsAlreadyVisited.length)
            nextGrid = Grid.setSeed[N, C, G](grid = nextGrid, seed = seed2)
            var neighbor: C = neighborsAlreadyVisited(randomIndex2)
            // current = Some(Cell.setLinked[N, C](cell = current.get, linked = current.get.linked ++ Set(neighbor.coords)))
            // neighbor = Cell.setLinked[N, C](cell = neighbor, linked = neighbor.linked ++ Set(current.get.coords))
            current = Some(current.get.setLinked[N, C](linked = current.get.linked ++ Set(neighbor.coords)))
            neighbor = neighbor.setLinked[N, C](linked = neighbor.linked ++ Set(current.get.coords))
            nextGrid = nextGrid.set[G](current.get).set(neighbor)
            visited = visited ++ Seq(current.get.coords)
          } else {
            current = None 
          }
        } 
      }
    }
    nextGrid
  }
}
