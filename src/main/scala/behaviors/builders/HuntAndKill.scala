package maze.behaviors.builders

import maze.classes.{ Coordinates, Cell, Grid }
import maze.classes.MazeType._
// import maze.behaviors.{ Linkage, Cell, Grid }
import maze.behaviors.Linkage

import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG

import scala.reflect.ClassTag

trait HuntAndKill extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE
  
  override def generate(grid: Grid): Grid = {
    var nextGrid: Grid = grid
    var visited: Seq[Coordinates] = Nil
    val (randomIndex1, seed1): (Int, RNG) = nextGrid.randomInt(nextGrid.size())
    nextGrid = nextGrid.set(seed = seed1)
    var current: Option[Cell] = Some(nextGrid.flatten()(randomIndex1))
    while (current != None) {
      val unvisitedNeighbors: Seq[Cell] = nextGrid.unlinkedNeighbors(current.get).filter(c => !visited.contains(c.coords))
      if (unvisitedNeighbors.nonEmpty) {
        // ****** KILL ****** 
        val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(unvisitedNeighbors.length)
        nextGrid = nextGrid.set(seed = seed2)
        var neighbor: Cell = unvisitedNeighbors(randomIndex2)
        current = Some(current.get.setLinked(linked = current.get.linked ++ Set(neighbor.coords)))
        neighbor = neighbor.setLinked(linked = neighbor.linked ++ Set(current.get.coords))
        nextGrid = nextGrid.set(current.get).set(neighbor)
        visited = visited ++ Seq(current.get.coords) ++ Seq(neighbor.coords)
        current = Some(neighbor)
      } else {
        // ****** HUNT ****** 
        current = nextGrid.find(c => { 
          !visited.contains(c.coords) && 
          nextGrid.unlinkedNeighbors(c).count(c2 => !nextGrid.get(c2).linked.isEmpty && visited.contains(c2.coords)) > 0
        })
        if (current.isDefined) {
          val neighborsAlreadyVisited: Seq[Cell] = nextGrid.unlinkedNeighbors(current.get).filter(c => visited.contains(c.coords))
          if (neighborsAlreadyVisited.length > 0) {
            val (randomIndex2, seed2): (Int, RNG) = nextGrid.randomInt(neighborsAlreadyVisited.length)
            nextGrid = nextGrid.set(seed = seed2)
            var neighbor: Cell = neighborsAlreadyVisited(randomIndex2)
            current = Some(current.get.setLinked(linked = current.get.linked ++ Set(neighbor.coords)))
            neighbor = neighbor.setLinked(linked = neighbor.linked ++ Set(current.get.coords))
            nextGrid = nextGrid.set(current.get).set(neighbor)
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
