package maze.behaviors

import maze.behaviors.Linkage
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
  }

  "BinaryTree" should "generate a 5x5 maze using Binary Tree and print it to screen" in {
    Given("5x5 grid")
    val grid = Grid(5, 5)
    When("generating maze using BinaryTree")
    val generated: Grid = module.build(grid)
    Then("the maze should be printed to screen")
    println(generated.toString())
  }


  it should "generate a 20x20 maze and print to screen" in {
    Given("20x20 grid")
    val grid = Grid(20, 20)
    When("generating maze using BinaryTree")
    val generated: Grid = module.build(grid)
    Then("the maze should be printed to screen")
    println(generated.toString())
  }

  it should "generate a 8x8 maze and print to screen" in {
    Given("8x8 grid")
    val grid = Grid(8, 8)
    When("generating maze using BinaryTree")
    val generated: Grid = module.build(grid)
    Then("the maze should be printed to screen")
    println(generated.toString())
  }

  it should "generate a 10x10 maze and print to screen" in {
    Given("10x10 grid")
    val grid = Grid(10, 10)
    When("generating maze using BinaryTree")
    val generated: Grid = module.build(grid)
    Then("the maze should be printed to screen")
    println(generated.toString())
  }

  it should "generate a 11x11 maze and print to screen" in {
    Given("11x11 grid")
    val grid = Grid(11, 11)
    When("generating maze using BinaryTree")
    val generated: Grid = module.build(grid)
    Then("the maze should be printed to screen")
    println(generated.toString())
  }

}