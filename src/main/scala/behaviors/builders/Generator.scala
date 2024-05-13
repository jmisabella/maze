package maze.behaviors.builders

import maze.classes.Grid

trait Generator {
  def generate(grid: Grid): Grid
}
