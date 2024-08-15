package maze.classes

import maze.classes.{ Cell, Grid }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ Sidewinder, BinaryTree, Generator }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class GridSpec extends AnyFlatSpec with GivenWhenThen {
  
  "Grid" should "initialize a 3x3 grid" in {
    When("initializing a 3x3 grid") 
    val grid = Grid(3, 3, Coordinates(0, 2), Coordinates(2, 0))
    Then("grid should have 3 rows and 3 columns")
    grid.cells.length should be (3)   
    for (row <- grid.cells) {
      for (cell <- row) {
        println(cell)
      }
    }
  }
  
  it should "determine distances from upper-left cell to all others in a 12x12 maze generated using Sidewinder" in {
    case object module extends Sidewinder {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("12x12 grid generated using Sidewinder")
    val unlinked = Grid(12, 12, Coordinates(0, 11), Coordinates(11, 0))
    val grid: Grid = module.generate(unlinked)
    println(grid) 
    println(grid.asci()) 
    When("determining distances from upper-left cell to each other cell")
    val result = module.distance.distances(grid, 0, 0)
    Then("all cells linked to (0, 0) should have non-empty values")
    for (cell <- result) {
      if (cell.linked.contains(Coordinates(0, 0))) {
        cell.value.trim() shouldNot be (empty)
      }
    }
    println(result)
  }
  
  it should "determine distances from upper-left cell to all others in a 12x12 maze generated using Binary Tree" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("12x12 grid generated using Binary Tree")
    val unlinked = Grid(12, 12, Coordinates(0, 11), Coordinates(11, 0))
    val grid: Grid = module.generate(unlinked)
    println(grid) 
    println(grid.asci()) 
    When("determining distances from upper-left cell to each other cell")
    val result = module.distance.distances(grid, 0, 0)
    Then("all cells linked to (0, 0) should have non-empty values")
    for (cell <- result) {
      if (cell.linked.contains(Coordinates(0, 0))) {
        cell.value.trim() shouldNot be (empty)
      }
    }
    println(result)
  }

  it should "use hard-coded unicode box characters to display to screen" in {
    val horizontalLine: String = "\u2501"
    val verticalLine: String = "\u2503"
    val fourWayJuncture: String = "\u254B"
    val threeWayJunctureNorthward: String = "\u253B"
    val threeWayJunctureEastward: String = "\u2523"
    val threeWayJunctureSouthward: String = "\u2533"
    val threeWayJunctureWestward: String = "\u252B"
    val upperLeftCorner: String = "\u250F"
    val upperRightCorner: String = "\u2513"
    val bottomRightCorner: String = "\u251B"
    val bottomLeftCorner: String = "\u2517"
    val cellWidth = 3
    val cell: String = " " * cellWidth
    var top: String = ""
    var middle: String = ""
    var bottom: String = ""

    top += upperLeftCorner + horizontalLine * cellWidth + threeWayJunctureSouthward
    top += horizontalLine * cellWidth + horizontalLine
    top += horizontalLine * cellWidth + upperRightCorner
    top += "\n"
    middle += verticalLine + cell + verticalLine
    middle += cell + " "
    middle += cell + verticalLine
    middle += "\n"
    bottom += bottomLeftCorner + horizontalLine * cellWidth + threeWayJunctureNorthward
    bottom += horizontalLine * cellWidth + horizontalLine
    bottom += horizontalLine * cellWidth + bottomRightCorner
    bottom += "\n"

    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(top + middle + bottom)
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  }

  it should "honor start and goal coordinates specified when generating a square grid" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x5 BinaryTree request")
    val request = MazeRequest(5, 5, Algorithm.BinaryTree, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    var grid: Grid = Generator.generate(request)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    grid.startCoords should equal (Coordinates(0, 4))
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    grid.goalCoords should equal (Coordinates(4, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    Then("grid's start cell at 0,4 should have isStart set to true")
    grid.get(0, 4).coords should equal (Coordinates(0, 4))
    grid.get(0, 4).coords.x should equal (0)
    grid.get(Coordinates(0, 4)).coords should equal (Coordinates(0, 4))
    grid.get(Coordinates(0, 4)).coords.x should equal (0)
    Then("grid's 0,0 cell should have isStart set to false")
    grid.get(0, 0).isStart should be (false)
    Then("grid's goal cell at 4,0 should have isGoal set to true")
    grid.get(4, 0).coords should equal (Coordinates(4, 0))
    grid.get(4, 0).coords.x should equal (4)
    grid.get(0, 4).isStart should be (true)
    grid.get(4, 0).isGoal should be (true)
    Then("grid's start cell at 0,4 should have isGoal set to false")
    grid.get(0, 4).isGoal should be (false)
    info(grid.asci())
    info(grid.toString())
  }
  
  it should "honor start and goal coordinates specified when generating a square grid using Generator, version 2" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x5 BinaryTree request")
    val request = MazeRequest(5, 5, Algorithm.BinaryTree, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    info("\n" + grid.asci())
  }
  
  it should "honor start and goal coordinates specified when generating a non-square grid using Generator" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x10 BinaryTree request")
    val request = MazeRequest(5, 10, Algorithm.BinaryTree, Coordinates(0, 9), Coordinates(4, 0))
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    info("START: " + grid.startCoords.toString())
    info("GOAL: " + grid.goalCoords.toString())
    info("REQUEST WIDTH: " + request.width.toString())
    info("REQUEST HEIGHT: " + request.height.toString())
    info("GRID WIDTH: " + grid.columns.toString())
    info("GRID HEIGHT: " + grid.rows.toString())
    grid.startCoords should equal (Coordinates(0, 9))
    val southwest = Coordinates(0, 9)
    val northeast = Coordinates(4, 0) 
    Then("grid width should be 5")
    grid.columns should equal (5)
    Then("grid height should be 10")
    grid.rows should equal (10)
    Then("grid's start should be southwest")
    grid.startCoords should equal (southwest)
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (northeast)
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    grid.get(southwest).coords should equal (southwest) // BUG: SHOULDN'T HAVE TO INVERSE THESE COORDS FOR THIS TEST TO PASS
    grid.get(0, 9).isStart should be (true)
    grid.get(4, 0).isGoal should be (true)
    info("EXPECTED (0,9), ACTUAL: " + grid.get(0, 9).coords.toString())
    info("EXPECTED (0,9), ACTUAL: " + grid.cells(9)(0).coords.toString())
    grid.get(0, 9).coords should equal (Coordinates(0, 9))
    grid.get(4, 0).coords should equal (Coordinates(4, 0))
    grid.get(southwest).isStart should be (true)
    grid.get(northeast).isGoal should be (true)
    info("\n" + grid.asci())
    info("\n" + grid.toString())
  }
  
  it should "honor start and goal coordinates specified in MazeRequest when generating a non-square grid" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x10 BinaryTree request")
    val request = MazeRequest(5, 10, Algorithm.BinaryTree, Coordinates(0, 9), Coordinates(4, 0))
    When("generating the grid")
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
  
  it should "honor start and goal coordinates specified in MazeRequest when generating a large non-square grid" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("50x100 BinaryTree request")
    val request = MazeRequest(50, 100, Algorithm.BinaryTree, Coordinates(0, 9), Coordinates(4, 0))
    When("generating the grid")
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