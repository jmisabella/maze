package maze.behaviors.solvers

import maze.classes.{ Grid, Cell, Coordinates }
import maze.behaviors.Distance
import maze.behaviors.solvers.Solver

trait Sidewinder extends Solver {

  type DISTANCE <: Distance
  val distance: DISTANCE

  // TODO: test 
  override def solve(grid: Grid, startX: Int, startY: Int, goalX: Int, goalY: Int): Grid = {
    distance.showPathTo(grid, startX, startY, goalX, goalY) 
  }
}