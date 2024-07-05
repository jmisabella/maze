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
    // for (cell <- grid.flatten()) {
    //   if (!module.linker.reachable(grid, 0, 0, cell.coords.x, cell.coords.y)) {
    //     println(s"CELL ${cell} IS UNREACHABLE")
    //   }
    // }
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

  it should "default to having southwest start and northeast goal" in {
    case object module extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x5 BinaryTree request")
    val request = MazeRequest(5, 5, Algorithm.BinaryTree, MazeType.Unsolved, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
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
    val request = MazeRequest(5, 5, Algorithm.BinaryTree, MazeType.Unsolved, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    var grid: Grid = Generator.generate(request)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    Then("grid's start cell at 0,4 should have isStart set to true")
    grid.get(0, 4).isStart should be (true)
    Then("grid's 0,0 cell should have isStart set to false")
    grid.get(0, 0).isStart should be (false)
    Then("grid's goal cell at 4,0 should have isGoal set to true")
    grid.get(4, 0).isGoal should be (true)
    Then("grid's start cell at 0,4 should have isGoal set to false")
    grid.get(0, 4).isGoal should be (false)
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
    val request = MazeRequest(5, 5, Algorithm.BinaryTree, MazeType.Unsolved, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    // val empty = Grid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    // val grid: Grid = module.generate(empty)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    // BUG // TODO: mazes generated from a MazeRequest are mixing up width and height
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
    val request = MazeRequest(10, 5, Algorithm.BinaryTree, MazeType.Unsolved, Coordinates(4, 0), Coordinates(0, 9))
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    // val empty = Grid(10, 5, Coordinates(0, 9), Coordinates(4, 0))
    // val grid: Grid = module.generate(empty)
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    // BUG // TODO: mazes generated from a MazeRequest are mixing up width and height
    info("\n" + grid.asci())
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
    val request = MazeRequest(5, 10, Algorithm.BinaryTree, MazeType.Unsolved, Coordinates(4, 0), Coordinates(0, 9))
    When("generating the grid")
    // BUG // TODO: Generator.generate(Grid) preserves correct x,y coords in Cells
    // BUG // TODO: however Generator.generate(MazeRequest) incorrectly mixes up x,y coords in Cells
    var grid: Grid = Generator.generate(request)
    // Then("grid's start should be southwest")
    // grid.startCoords should equal (Coordinates(0, request.height - 1))
    // Then("grid's goal should be northeast")
    // grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    // Then("exactly one cell should be the goal cell (isGoal == true)")
    // grid.count(c => c.isGoal) should equal (1)
    // Then("exactly one cell should be the starting cell (isStart == true)")
    // grid.count(c => c.isStart) should equal (1)
    // Then("grid's start cell at 0,9 should have isStart set to true")
    // grid.get(0, 9).isStart should be (true)
    // Then("grid's 0,0 cell should have isStart set to false")
    // grid.get(0, 0).isStart should be (false)
    // Then("grid's goal cell at 4,0 should have isGoal set to true")
    // grid.get(4, 0).isGoal should be (true)
    // Then("grid's start cell at 0,4 should have isGoal set to false")
    // grid.get(0, 4).isGoal should be (false)
    info("\n" + grid.asci())
    info(grid.toString())
  }
  
  // it should "honor start and goal coordinates (specified during Grid construction) when generating a non-square grid using BinaryTree" in {
  //   case object module extends BinaryTree {
  //     case object _linkage extends Linkage
  //     override type LINKAGE = Linkage
  //     override val linker = _linkage
  //     case object _distance extends Distance
  //     override type DISTANCE = Distance
  //     override val distance = _distance
  //   }
  //   Given("5x10 grid")
  //   // BUG // TODO: Generator.generate(Grid) preserves correct x,y coords in Cells
  //   // BUG // TODO: however Generator.generate(MazeRequest) incorrectly mixes up x,y coords in Cells
  //   val grid = Grid(10, 5, Coordinates(0, 9), Coordinates(4, 0))
  //   When("generating the grid")
  //   Then("grid's start should be southwest")
  //   grid.startCoords should equal (Coordinates(0, 9))
  //   Then("grid's goal should be northeast")
  //   grid.goalCoords should equal (Coordinates(4, 0))
  //   When("generating maze using BinaryTree")
  //   // BUG // TODO: when generating maze, cells aren't updated regarding isStart/isGoal
  //   val generated: Grid = module.generate(grid)
  //   Then("exactly one cell should be the goal cell (isGoal == true)")
  //   generated.count(c => c.isGoal) should equal (1)
  //   // info(generated.filter(c => c.isGoal).toString())
  //   // Then("exactly one cell should be the starting cell (isStart == true)")
  //   // generated.count(c => c.isStart) should equal (1)
  //   // Then("grid's start cell at 0,9 should have isStart set to true")
  //   // generated.get(0, 9).isStart should be (true)
  //   // Then("grid's 0,0 cell should have isStart set to false")
  //   // generated.get(0, 0).isStart should be (false)
  //   // Then("grid's goal cell at 4,0 should have isGoal set to true")
  //   // generated.get(4, 0).isGoal should be (true)
  //   // Then("grid's start cell at 0,4 should have isGoal set to false")
  //   // generated.get(0, 4).isGoal should be (false)
  //   // info(generated.asci())
  //   // info(generated.toString())
  // }
  
  // it should "honor start and goal coordinates (specified during Grid construction) when generating a non-square grid using Sidewinder" in {
  //   case object module extends Sidewinder {
  //     case object _linkage extends Linkage
  //     override type LINKAGE = Linkage
  //     override val linker = _linkage
  //     case object _distance extends Distance
  //     override type DISTANCE = Distance
  //     override val distance = _distance
  //   }
  //   Given("5x10 grid")
  //   // BUG // TODO: Generator.generate(Grid) preserves correct x,y coords in Cells
  //   // BUG // TODO: however Generator.generate(MazeRequest) incorrectly mixes up x,y coords in Cells
  //   val grid = Grid(10, 5, Coordinates(0, 9), Coordinates(4, 0))
  //   When("generating the grid")
  //   Then("grid's start should be southwest")
  //   grid.startCoords should equal (Coordinates(0, 9))
  //   Then("grid's goal should be northeast")
  //   grid.goalCoords should equal (Coordinates(4, 0))
  //   When("generating maze using BinaryTree")
  //   // BUG // TODO: when generating maze, cells aren't updated regarding isStart/isGoal
  //   val generated: Grid = module.generate(grid)
  //   // Then("exactly one cell should be the goal cell (isGoal == true)")
  //   // generated.count(c => c.isGoal) should equal (1)
  //   // info(generated.filter(c => c.isGoal).toString())
  //   // Then("exactly one cell should be the starting cell (isStart == true)")
  //   // generated.count(c => c.isStart) should equal (1)
  //   // Then("grid's start cell at 0,9 should have isStart set to true")
  //   // generated.get(0, 9).isStart should be (true)
  //   // Then("grid's 0,0 cell should have isStart set to false")
  //   // generated.get(0, 0).isStart should be (false)
  //   // Then("grid's goal cell at 4,0 should have isGoal set to true")
  //   // generated.get(4, 0).isGoal should be (true)
  //   // Then("grid's start cell at 0,4 should have isGoal set to false")
  //   // generated.get(0, 4).isGoal should be (false)
  //   info(generated.asci())
  //   // info(generated.toString())
  // }

}