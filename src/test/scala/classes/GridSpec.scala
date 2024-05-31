package maze.classes

import maze.classes.{ Cell, Grid }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ Sidewinder, BinaryTree }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class GridSpec extends AnyFlatSpec with GivenWhenThen {
  
  "Grid" should "initialize a 3x3 grid" in {
    When("initializing a 3x3 grid") 
    val grid = Grid(3, 3)
    Then("grid should have 3 rows and 3 columns")
    grid.cells.length should be (3)   
    for (row <- grid.cells) {
      for (cell <- row) {
        println(cell)
      }
    }
  }

  // it should "update cell to be visited" in {
  //   Given("3x3 grid with all unvisited cells") 
  //   val grid = Grid(3, 3)
  //   grid.count(c => !c.visited) should be (9)
  //   When("updating first cell to be visited")
  //   val first: Cell = grid.get(0)(0).copy(visited = true)
  //   val updatedGrid: Grid = grid.set(first)
  //   Then("first cell in grid is only cell in grid which has been visited")
  //   updatedGrid.get(0)(0).visited should be (true)
  //   updatedGrid.count(c => c.visited) should be (1)
  //   updatedGrid.count(c => !c.visited) should be (8)
  // }

  // it should "flatten and unflatten its grid to end up with original X by Y matrix" in {
  //   Given("5x5 grid with all unvisited cells") 
  //   val grid: Grid = Grid(5, 5)
  //   When("flattening grid into a list")
  //   val flattened: Seq[Cell] = grid.flatten()
  //   Then("flattened list's length should equal original grid's rows multiplied by columns")
  //   flattened.length should equal (grid.rows * grid.columns)
  //   When("unflattening back to a grid")
  //   val unflattened: Grid = grid.unflatten(flattened)
  //   Then("the unflattened grid should equal the original grid")
  //   grid.rows should equal (unflattened.rows)
  //   grid.columns should equal (unflattened.columns)
  //   grid.cells should equal (unflattened.cells)
  //   When("updating middle cell to be visited and flattening and re-unflattening cells")
  //   val middle: Cell = unflattened.get(2)(2).copy(visited = true)
  //   middle.visited should be (true)
  //   val grid2 = unflattened.set(middle)
  //   grid2.get(2)(2).visited should be (true)
  //   val unflattened2: Grid = grid2.unflatten(grid2.flatten())
  //   Then("the unflattend grid should equal the original, such that only the middle cell has been visited")
  //   grid2.rows should equal (unflattened2.rows)
  //   grid2.columns should equal (unflattened2.columns)
  //   grid2.cells should equal (unflattened2.cells)
  // }
  
  // it should "determine distances from upper-left cell to all others in a 5x5 maze generated using Sidewinder" in {
  //   case object module extends Sidewinder {
  //     case object _linkage extends Linkage
  //     override type LINKAGE = Linkage
  //     override val linker = _linkage
  //     case object _distance extends Distance
  //     override type DISTANCE = Distance
  //     override val distance = _distance
  //   }
  //   Given("5x5 grid generated using Sidewinder")
  //   val unlinked = Grid(5, 5)
  //   val grid: Grid = module.generate(unlinked)
  //   When("determining distances from upper-left cell to each other cell")
  //   val result = module.distance.distances(grid, 0, 0)
  //   Then("all cells linked to (0, 0) should have non-empty values")
  //   for (cell <- result) {
  //     if (cell.linked.contains(Coordinates(0, 0))) {
  //       cell.value.trim() shouldNot be (empty)
  //     }
  //   }
  //   println(result)
  // }
  
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
    val unlinked = Grid(12, 12)
    val grid: Grid = module.generate(unlinked)
    println(grid) 
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
    val unlinked = Grid(12, 12)
    val grid: Grid = module.generate(unlinked)
    println(grid) 
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


}