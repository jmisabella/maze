package maze.behaviors

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ Sidewinder, BinaryTree }
import maze.classes.{ Cell, Grid, Coordinates }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class DistanceSpec extends AnyFlatSpec with GivenWhenThen {

  case object sidewinder extends Sidewinder {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage
    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }
  
  case object binaryTree extends BinaryTree {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage

    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }
  
  
  "Distance" should "generate a 5x5 maze using Sidewinder and determine distances from upper-left cell to all other reachable cells before printing to screen" in {
    Given("5x5 grid generated using Sidewinder")
    val unlinked = Grid(5, 5)
    val grid: Grid = sidewinder.generate(unlinked)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.showDistances(grid, 0, 0)
    println(result)
  }

  it should "generate a 5x5 maze using Sidewinder and show shortest path from upper-left cell to botom-right cell before printing to screen" in {
    Given("5x5 grid generated using Sidewinder")
    val unlinked = Grid(5, 5)
    val grid: Grid = sidewinder.generate(unlinked)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.showPathTo(grid, 0, 0, 4, 4)
    println(result)
  }

  it should "generate a 5x5 maze using Sidewinder and determine shortest path from upper-left cell to botom-right cell" in {
    Given("5x5 grid generated using Sidewinder")
    val grid: Grid = sidewinder.generate(5, 5)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.pathTo(grid, 0, 0, 4, 4)
    info(sidewinder.distance.showPathTo(grid, 0, 0, 4, 4).toString)
    val cellCountOnShortestPath: Int = grid.count(c => result.keySet.contains(c.coords))
    cellCountOnShortestPath shouldBe > (0)
    cellCountOnShortestPath shouldBe >= (4 + 4)
  }
  
  it should "generate a 5x5 maze using Binary Tree and determine distances from upper-left cell to all other reachable cells before printing to screen" in {
    Given("5x5 grid generated using BinaryTree")
    val unlinked = Grid(5, 5)
    val grid: Grid = binaryTree.generate(unlinked)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.showDistances(grid, 0, 0)
    println(result)
  }

  it should "generate a 5x5 maze using Sidewinder and have bottom right corner cell and its left neighbor isolated from rest of grid" in {
    Given("5x5 grid generated using Sidewinder with bottom right corner cell and cell to its left isolated from the rest of the grid")
    val unlinked = Grid(5, 5)
    var grid: Grid = sidewinder.generate(unlinked)
    // bottom right corner cell and cell to its left are linked to each other but isolated from all other cells
    var bottomRightCell: Cell = (grid.get(4)(4))//.copy(linked = Set(Coordinates(4, 3)))
    var leftOFBottomRightCell: Cell = (grid.get(4)(3))//.copy(linked = Set(Coordinates(4, 4)))
    def unlinkUnwantedCells(cell: Cell, unwanted: Seq[Cell]): Cell = {
      cell.copy(linked = cell.linked.filter(c => unwanted.count(c2 => c2.coords == c) == 0))
    }
    val unreachables: Seq[Cell] = Seq(bottomRightCell, leftOFBottomRightCell)
    var cell: Cell = grid.get(4)(2)
    cell = unlinkUnwantedCells(cell, unreachables)
    grid = grid.set(cell)
    cell = grid.get(3)(3)
    cell = unlinkUnwantedCells(cell, unreachables)
    grid = grid.set(cell)
    cell = grid.get(3)(4)
    cell = unlinkUnwantedCells(cell, unreachables)
    grid = grid.set(cell)
    bottomRightCell = bottomRightCell.copy(linked = Set(Coordinates(4, 3)))
    info("BBBBBB: " + bottomRightCell.linked)
    leftOFBottomRightCell = leftOFBottomRightCell.copy(linked = Set(Coordinates(4, 4)))
    info("AAAAAAA: " + leftOFBottomRightCell.linked)
    grid = grid.set(leftOFBottomRightCell)
    grid = grid.set(bottomRightCell) 
    info("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")
    info(grid.toString())
    info("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")
    When("determining whether bottom right corner cell is reachable from the cell to its left")
    Then("these 2 cells are reachable from each other")
    grid.reachable(4, 3, 4, 4) shouldBe (true)
    When("determining whether bottom right corner cell is reachable from the upper left corner cell")
    Then("these 2 cells are unreachable from each other")
    grid.reachable(0, 0, 4, 3) shouldBe (false)
    When("deisolating cells in the grid")
    grid = sidewinder.deisolateCells(grid)
    Then("bottom right corner cell is still reachable from the cell to its left")
    grid.reachable(4, 3, 4, 4) shouldBe (true)
    Then("bottom right corner cell should now be reachable from upper left corner cell")
    grid.reachable(0, 0, 4, 4) shouldBe (true)
  }


}
