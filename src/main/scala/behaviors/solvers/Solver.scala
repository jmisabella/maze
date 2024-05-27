package maze.behaviors.solvers

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.{ Linkage, Distance }

trait Solver {
  
  type LINKAGE <: Linkage
  val linker: LINKAGE

  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def solve(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int): Grid
  def solve(grid: Grid, start: Coordinates, goal: Coordinates): Grid = solve(grid, start.x, start.y, goal.x, goal.y)

}
