package maze.behaviors.solvers

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.{ Linkage, Distance }

trait Solver {
  
  type DISTANCE <: Distance
  val distance: DISTANCE
 
  def solve(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int, overrideChar: Option[Char] = None): Grid
  def solve(grid: Grid, start: Coordinates, goal: Coordinates, overrideChar: Option[Char]): Grid = solve(grid, start.x, start.y, goal.x, goal.y, overrideChar)

}
