package maze.behaviors

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Generator }
import maze.classes.{ SquareCell, SquareGrid, Coordinates, MazeRequest, Algorithm }
import maze.classes.MazeType._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class BinaryTreeSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends BinaryTree[SquareCell, SquareGrid] {
    case object _linkage extends Linkage[SquareCell, SquareGrid]
    override type LINKAGE = Linkage[SquareCell, SquareGrid]
    override val linker = _linkage

    case object _distance extends Distance[SquareCell, SquareGrid]
    override type DISTANCE = Distance[SquareCell, SquareGrid]
    override val distance = _distance
  }

  "BinaryTree" should "generate a 5x5 maze using Binary Tree and print it to screen" in {
    Given("5x5 grid")
    val grid = SquareGrid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    When("generating maze using BinaryTree")
    val generated = module.generate(grid)
    Then("generated maze should have height of 5 cells")
    generated.height should be (5)
    generated.cells.length should be (5)
    Then("generated maze should have width of 5 cells")
    generated.width should be (5)
    generated.cells.count(c => c.length == 5) should be (5)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }


  it should "generate a 20x20 maze and print to screen" in {
    Given("20x20 grid")
    val grid = SquareGrid(20, 20, Coordinates(0, 19), Coordinates(19, 0))
    When("generating maze using BinaryTree")
    val generated = module.generate(grid)
    Then("generated maze should have height of 20 cells")
    generated.height should be (20)
    generated.cells.length should be (20)
    Then("generated maze should have width of 20 cells")
    generated.width should be (20)
    generated.cells.count(c => c.length == 20) should be (20)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  it should "generate a 8x8 maze and print to screen" in {
    Given("8x8 grid")
    val grid = SquareGrid(8, 8, Coordinates(0, 7), Coordinates(7, 0))
    When("generating maze using BinaryTree")
    val generated = module.generate(grid)
    Then("generated maze should have height of 8 cells")
    generated.height should be (8)
    generated.cells.length should be (8)
    Then("generated maze should have width of 8 cells")
    generated.width should be (8)
    generated.cells.count(c => c.length == 8) should be (8)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  it should "generate a 10x10 maze and print to screen" in {
    Given("10x10 grid")
    val grid = SquareGrid(10, 10, Coordinates(0, 9), Coordinates(9, 0))
    When("generating maze using BinaryTree")
    val generated = module.generate(grid)
    Then("generated maze should have height of 10 cells")
    generated.height should be (10)
    generated.cells.length should be (10)
    Then("generated maze should have width of 10 cells")
    generated.width should be (10)
    generated.cells.count(c => c.length == 10) should be (10)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }

  it should "generate a 11x11 maze and print to screen" in {
    Given("11x11 grid")
    val grid = SquareGrid(11, 11, Coordinates(0, 10), Coordinates(10, 0))
    When("generating maze using BinaryTree")
    val generated = module.generate(grid)
    Then("generated maze should have height of 11 cells")
    generated.height should be (11)
    generated.cells.length should be (11)
    Then("generated maze should have width of 11 cells")
    generated.width should be (11)
    generated.cells.count(c => c.length == 11) should be (11)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
  }
  
  it should "honor start and goal coordinates specified in MazeRequest when generating a large non-square grid" in {
    case object module extends BinaryTree[SquareCell, SquareGrid] {
      case object _linkage extends Linkage[SquareCell, SquareGrid]
      override type LINKAGE = Linkage[SquareCell, SquareGrid]
      override val linker = _linkage
      case object _distance extends Distance[SquareCell, SquareGrid]
      override type DISTANCE = Distance[SquareCell, SquareGrid]
      override val distance = _distance
    }
    Given("JSON for a 52x29 BinaryTree request")
    val json = """{"mazeType":"Orthogonal","width":"52","height":"29","algorithm":"BinaryTree","startX":"14","startY":"0","goalX":"14","goalY":"28"}""" 
    When("generating the grid")
    val request: MazeRequest = MazeRequest(json)
    var grid = Generator.generate(request)
    Then("grid's start should be 14,0")
    grid.startCoords should equal (Coordinates(14, 0))
    info("START COORDS: " + grid.startCoords.toString())
    Then("grid's goal should be 14,28")
    grid.goalCoords should equal (Coordinates(14, 28))
    Then("resulting maze is a perfect maze")
    grid.isPerfectMaze() shouldBe (true) 
    info("\n" + grid.asci())
    info(grid.toString())
  }

}