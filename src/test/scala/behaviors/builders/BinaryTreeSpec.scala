package maze.behaviors

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Generator }
import maze.classes.{ Cell, Grid, Coordinates, MazeRequest, Algorithm }

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
    val grid = Grid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
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
    val grid = Grid(20, 20, Coordinates(0, 19), Coordinates(19, 0))
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
    val grid = Grid(8, 8, Coordinates(0, 7), Coordinates(7, 0))
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
    val grid = Grid(10, 10, Coordinates(0, 9), Coordinates(9, 0))
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
    val grid = Grid(11, 11, Coordinates(0, 10), Coordinates(10, 0))
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
  
  it should "honor start and goal coordinates specified in MazeRequest when generating a large non-square grid" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("JSON for a 52x29 BinaryTree request")
    val json = """{"width":"52","height":"29","algorithm":"BinaryTree","startX":"14","startY":"0","goalX":"14","goalY":"28","mazeType":"Solved"}""" 
    When("generating the grid")
    val request: MazeRequest = MazeRequest(json)
    var grid: Grid = Generator.generate(request)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    info("START COORDS: " + grid.startCoords.toString())
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    Then("grid's start cell at 9,0 should have isStart set to true")
    grid.get(0, 9).isStart should be (true)
    Then("grid's 0,0 cell should have isStart set to false")
    grid.get(0, 0).isStart should be (false)
    Then("grid's goal cell at 0,4 should have isGoal set to true")
    grid.get(4, 0).isGoal should be (true)
    Then("grid's start cell at 9,0 should have isGoal set to false")
    grid.get(0, 9).isGoal should be (false)
    info("\n" + grid.asci())
    info(grid.toString())
  }

}