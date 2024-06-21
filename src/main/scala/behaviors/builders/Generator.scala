package maze.behaviors.builders

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.{ Linkage, Distance }

trait Generator {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def generate(grid: Grid): Grid
  def generate(x: Int, y: Int): Grid = generate(Grid(x, y))

}
