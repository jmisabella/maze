package maze.behaviors

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.BinaryTree
import maze.classes.{ Cell, Grid }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BinaryTreeSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends BinaryTree {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage

    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }

  "BinaryTree" should "generate a 5x5 maze using Binary Tree and print it to screen" in {
    Given("5x5 grid")
    val grid = Grid(5, 5)
    When("generating maze using BinaryTree")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 5 cells")
    generated.rows should be (5)
    generated.cells.length should be (5)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (5)
    generated.cells.count(c => c.length == 5) should be (5)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }


  it should "generate a 20x20 maze and print to screen" in {
    Given("20x20 grid")
    val grid = Grid(20, 20)
    When("generating maze using BinaryTree")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 20 cells")
    generated.rows should be (20)
    generated.cells.length should be (20)
    Then("generated maze should have width of 20 cells")
    generated.columns should be (20)
    generated.cells.count(c => c.length == 20) should be (20)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  it should "generate a 8x8 maze and print to screen" in {
    Given("8x8 grid")
    val grid = Grid(8, 8)
    When("generating maze using BinaryTree")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 8 cells")
    generated.rows should be (8)
    generated.cells.length should be (8)
    Then("generated maze should have width of 8 cells")
    generated.columns should be (8)
    generated.cells.count(c => c.length == 8) should be (8)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  it should "generate a 10x10 maze and print to screen" in {
    Given("10x10 grid")
    val grid = Grid(10, 10)
    When("generating maze using BinaryTree")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 10 cells")
    generated.rows should be (10)
    generated.cells.length should be (10)
    Then("generated maze should have width of 10 cells")
    generated.columns should be (10)
    generated.cells.count(c => c.length == 10) should be (10)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  it should "generate a 11x11 maze and print to screen" in {
    Given("11x11 grid")
    val grid = Grid(11, 11)
    When("generating maze using BinaryTree")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 11 cells")
    generated.rows should be (11)
    generated.cells.length should be (11)
    Then("generated maze should have width of 11 cells")
    generated.columns should be (11)
    generated.cells.count(c => c.length == 11) should be (11)

    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  //// TODO: this seems to result in infinite loop
  // it should "always generate a maze in which each cell is accessible from the upper-left corner cell" in {
  //   Given("20x20 grid")
  //   val empty = Grid(20, 20)
  //   When("generating maze using BinaryTree")
  //   val grid: Grid = module.generate(empty)
  //   Then("generated maze should have height of 20 cells")
  //   grid.rows should be (20)
  //   grid.cells.length should be (20)
  //   Then("generated maze should have width of 20 cells")
  //   grid.columns should be (20)
  //   grid.cells.count(c => c.length == 20) should be (20)
  //   Then("each cell should be accessible from all other cells")
  //   val dist = module.distance.distances(grid, 0, 0)
  //   for (cell <- grid) {
  //     dist.keySet should contain (cell.coords) 
  //   }
  // }

}