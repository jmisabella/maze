package maze.classes

import maze.behaviors.GridFactory
import maze.classes.{ Cell, Grid }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class GridSpec extends AnyFlatSpec with GivenWhenThen {
  
  case object module extends GridFactory

  "Grid" should "update cell to be visited" in {
    Given("3x3 grid with all unvisited cells") 
    val grid = module.initialize(3, 3)
    grid.count(c => !c.visited) should be (9)
    When("updating first cell to be visited")
    val first: Cell = grid.get(0)(0).copy(visited = true)
    val updatedGrid: Grid = grid.set(first)
    Then("first cell in grid is only cell in grid which has been visited")
    updatedGrid.get(0)(0).visited should be (true)
    updatedGrid.count(c => c.visited) should be (1)
    updatedGrid.count(c => !c.visited) should be (8)
  }

}