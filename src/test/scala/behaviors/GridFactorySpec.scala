package maze.behaviors

import maze.behaviors.GridFactory
import maze.classes.{ Cell, Grid }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class GridFactorySpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends GridFactory

  "GridPreparation" should "prepare a 3x3 grid" in {
    When("preparing a 3x3 grid") 
    val grid = module.prepareGrid(3, 3)
    Then("grid should have 3 rows and 3 columns")
    grid.cells.length should be (3)   
    for (row <- grid.cells) {
      for (cell <- row) {
        println(cell)
      }
    }
  }

  it should "initialize a 3x3 grid" in {
    When("initializing a 3x3 grid") 
    val grid = module.initialize(3, 3)
    Then("grid should have 3 rows and 3 columns")
    grid.cells.length should be (3)   
    for (row <- grid.cells) {
      for (cell <- row) {
        println(cell)
      }
    }
  }


}