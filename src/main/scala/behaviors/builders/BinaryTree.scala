package maze.behaviors.builders

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Linkage
import maze.utilities.RNG

trait BinaryTree {

  type LINKAGE <: Linkage
  val linker: LINKAGE

  def build(grid: Grid): Grid = {
    var nextGrid: Grid = grid // to keep track of next random seeds
    val unflattened: Seq[Seq[Cell]] = for (cell <- grid.flatten()) yield {
      val neighbors: Seq[Coordinates] = (cell.neighbors.north, cell.neighbors.east) match {
        case (None, None) => Nil
        case (Some(north), None) => Seq(north)
        case (None, Some(east)) => Seq(east)
        case (Some(north), Some(east)) => Seq(north, east)
      }
      neighbors match {
        case Nil => Seq(cell)
        case xs => {
          val (index, nextSeed): (Int, RNG) = nextGrid.seed.boundedPositiveInt(neighbors.length)
          nextGrid = nextGrid.copy(seed = nextSeed) // we made a random move, update grid's seed to the next seed
          val neighbor: Coordinates = neighbors(index)
          linker.link(Seq(cell, nextGrid.get(neighbor)))
        }
      }
    }
    // val flattened = unflattened.flatten
    // val strandeds: Seq[Cell] = flattened.filter(c => c.linked == Nil)
    // var unstrandeds: Seq[Cell] = flattened.filter(c => !c.linked.isEmpty) 
    // for (stranded <- strandeds) {
    //   val neighborsLinked: Seq[Cell] = flattened.filter(c => c.linked.contains(stranded.coords))
    //   for (neighbor <- neighborsLinked) {
    //     unstrandeds = unstrandeds ++ linker.link(Seq(neighbor, stranded)).filter(c => c.coords == stranded.coords)
    //   }
    // } 
    // nextGrid.unflatten(unstrandeds)
    println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    val result = nextGrid.unflatten(unflattened.flatten) //.filter(c => c.linked != Set()))
    for (cell <- result.cells.flatten) {
      println("CELL: " + cell)
    }
    // for (row <- result.cells) {
    //   println("\n\nROW: " + row.mkString)
    //   for (cell <- row) {
    //     println(cell.coords + " linked: " + cell.linked.mkString(", "))
    //   }
    // }
    result
  }

}
