package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.behaviors.builders.Generator
import maze.utilities.RNG
import maze.classes.Neighbors

trait Wilsons extends Generator {
  type LINKAGE <: Linkage
  val linker: LINKAGE

  // def getRandomPath(startCell: Cell, unvisited: Seq[Cell], random: RNG): (Seq[Cell], RNG) = {
  //   var path = Seq(startCell)
  //   var seed = random 
  //   while (unvisited.contains(startCell)) {
  //     val neighbors: Seq[Coordinates] = startCell.neighbors.toSeq
  //     val (randomIndex, seed1): (Int, RNG) = random.boundedPositiveInt(neighbors.length)
  //     seed = seed1
  //     val cell: Cell = unvisited.filter(c => c.coords == neighbors(randomIndex)).head
  //     if (!unvisited.contains(cell)) {
  //       // found rest of the maze
  //       path = path ++ Seq(cell)
  //       return (path, seed)
  //     } else if (path.contains(cell)) {
  //       // interset with self, remove loop from the path
  //       path = for (i <- 0 to path.indexOf(cell)) yield path(i)
  //     } else {
  //       path = path ++ Seq(cell)
  //     }
  //   }
  //   (path, seed)
  // }
  override def generate(grid: Grid): Grid = {

    ???
  }
  // override def generate(grid: Grid): Grid = {
  //   var flattenedResults: Seq[Cell] = Nil
  //   var nextGrid: Grid = grid
  //   var unvisited: Seq[Cell] = nextGrid.flatten()
  //   val (randomFirstIndex, seed1): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
  //   var first: Cell = unvisited(randomFirstIndex)
  //   nextGrid = nextGrid.copy(seed = seed1)
  //   unvisited = unvisited.filter(c => c.coords != first.coords)
  //   while (!unvisited.isEmpty) {
  //     val (randomCellIndex1, seed2): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
  //     var cell: Cell = unvisited(randomCellIndex1)
  //     nextGrid = nextGrid.copy(seed = seed2) 
  //     var path: Seq[Cell] = Seq(cell)
  //     while (unvisited.contains(cell)) {
  //       val (randomCellIndex2, seed3): (Int, RNG)  = nextGrid.randomInt(unvisited.length)
  //       cell = unvisited(randomCellIndex2)
  //       if (path.map(_.coords).contains(cell.coords)) {
  //         // loop erase
  //         path = for (i <- 0 to path.map(c => c.coords).indexOf(cell.coords)) yield path(i) // TODO: until (exclusive) or to (inclusive) ???
  //       } else {
  //         path = path ++ Seq(cell)
  //       }
  //     }
  //     // TODO: should this be to (inclusive) or until (exclusive) ???
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
  //     flattenedResults = flattenedResults ++ linkedBiDirectional
  //   }
  //   nextGrid.unflatten(flattenedResults)
  // }

}



