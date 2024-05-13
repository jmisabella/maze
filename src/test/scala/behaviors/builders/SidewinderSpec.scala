package maze.behaviors.builders

import maze.behaviors.Linkage
import maze.behaviors.builders.Sidewinder
import maze.classes.{ Cell, Grid }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class SidewinderSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends Sidewinder {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage
  }
  
  "Sidewinder" should "generate a 5x5 maze using Binary Tree and print it to screen" in {
    Given("5x5 grid")
    val grid = Grid(5, 5)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 5 cells")
    generated.rows should be (5)
    generated.cells.length should be (5)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (5)
    generated.cells.count(c => c.length == 5) should be (5)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("5x5 Sidewinder")
    println(generated.toString())
  }

  it should "generate a 25x25 maze using Binary Tree and print it to screen" in {
    Given("25x25 grid")
    val grid = Grid(25, 25)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 25 cells")
    generated.rows should be (25)
    generated.cells.length should be (25)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (25)
    generated.cells.count(c => c.length == 25) should be (25)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("25x25 Sidewinder")
    println(generated.toString())
  }

  it should "generate a 35x35 maze using Binary Tree and print it to screen" in {
    Given("35x35 grid")
    val grid = Grid(35, 35)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 35 cells")
    generated.rows should be (35)
    generated.cells.length should be (35)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (35)
    generated.cells.count(c => c.length == 35) should be (35)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("35x35 Sidewinder")
    println(generated.toString())
  }

}