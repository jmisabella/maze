package maze.behaviors

import maze.classes.{ Cell, Grid, Coordinates }
import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ BinaryTree, Sidewinder }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class LinkageSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends Linkage

  case object sidewinder extends Sidewinder {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage
    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }


  "Linkage" should "update cell to be visited" in {
    Given("3x3 grid with all unvisited cells") 
    val grid = Grid(3, 3)
    grid.count(c => !c.visited) should be (9)
    When("updating first cell to be visited")
    val first: Cell = module.visit(grid.get(0)(0))
    val updatedGrid: Grid = grid.set(first)
    Then("first cell in grid is only cell in grid which has been visited")
    updatedGrid.get(0)(0).visited should be (true)
    updatedGrid.count(c => c.visited) should be (1)
    updatedGrid.count(c => !c.visited) should be (8)
  }
  
  it should "bi-directionaly link bottom row of cells in a 4x4 grid" in {
    Given("4x4 grid") 
    val grid: Grid = Grid(4, 4)
    When("linking all cells in bottom row together")
    val bottomRow: Seq[Cell] = grid.row(3)
    val linkedBottomRow: Seq[Cell] = module.link(bottomRow)
    var updated: Grid = grid
    for (cell <- linkedBottomRow) {
      updated = updated.set(cell)
    }
    updated.get(0)(0).linked should be (empty)
    updated.get(0)(1).linked should be (empty)
    updated.get(0)(2).linked should be (empty)
    updated.get(0)(3).linked should be (empty)
    updated.get(1)(0).linked should be (empty)
    updated.get(1)(1).linked should be (empty)
    updated.get(1)(2).linked should be (empty)
    updated.get(1)(3).linked should be (empty)
    updated.get(2)(0).linked should be (empty)
    updated.get(2)(1).linked should be (empty)
    updated.get(2)(2).linked should be (empty)
    updated.get(2)(3).linked should be (empty)
    module.linked(updated.get(0)(0), updated.get(0)(1), bidi=true) should be (false)
    module.linked(updated.get(0)(1), updated.get(0)(2), bidi=true) should be (false)
    module.linked(updated.get(0)(2), updated.get(0)(3), bidi=true) should be (false)
    module.linked(updated.get(1)(0), updated.get(1)(1), bidi=true) should be (false)
    module.linked(updated.get(1)(1), updated.get(1)(2), bidi=true) should be (false)
    module.linked(updated.get(1)(2), updated.get(1)(3), bidi=true) should be (false)
    module.linked(updated.get(2)(0), updated.get(2)(1), bidi=true) should be (false)
    module.linked(updated.get(2)(1), updated.get(2)(2), bidi=true) should be (false)
    module.linked(updated.get(2)(2), updated.get(2)(3), bidi=true) should be (false)
    module.linked(updated.get(3)(0), updated.get(3)(1), bidi=true) should be (true)
    module.linked(updated.get(3)(1), updated.get(3)(2), bidi=true) should be (true)
    module.linked(updated.get(3)(2), updated.get(3)(3), bidi=true) should be (true)
  }

  it should "bi-directionaly link a zig-zag pattern upper-left to bottom-right of a 5x5 grid" in {
    Given("5x5 grid") 
    val originalGrid: Grid = Grid(5, 5)
    When("linking cells together in a zig-zag pattern from upper-left to bottom-right")
    def cells(g: Grid): Seq[Cell] = Seq(
          g.get(0)(0), 
          g.get(0)(1), 
          g.get(1)(1), 
          g.get(1)(2), 
          g.get(2)(2), 
          g.get(2)(3), 
          g.get(3)(3), 
          g.get(3)(4), 
          g.get(4)(4) )
    
    val linked: Seq[Cell] = module.link(cells(originalGrid))
    var updated: Grid = originalGrid
    for (cell <- linked) {
      updated = updated.set(cell)
    }
    Then("updated grid should reflect cells which are linked")
    updated.get(0)(0).linked should not be (empty)
    updated.get(0)(1).linked should not be (empty)
    updated.get(0)(2).linked should be (empty)
    updated.get(0)(3).linked should be (empty)
    updated.get(0)(4).linked should be (empty)
    updated.get(1)(0).linked should be (empty)
    updated.get(1)(1).linked should not be (empty)
    updated.get(1)(2).linked should not be (empty)
    updated.get(1)(3).linked should be (empty)
    updated.get(1)(4).linked should be (empty)
    updated.get(2)(0).linked should be (empty)
    updated.get(2)(1).linked should be (empty)
    updated.get(2)(2).linked should not be (empty)
    updated.get(2)(3).linked should not be (empty)
    updated.get(2)(4).linked should be (empty)
    updated.get(3)(0).linked should be (empty)
    updated.get(3)(1).linked should be (empty)
    updated.get(3)(2).linked should be (empty)
    updated.get(3)(3).linked should not be (empty)
    updated.get(3)(4).linked should not be (empty)
    updated.get(4)(0).linked should be (empty)
    updated.get(4)(1).linked should be (empty)
    updated.get(4)(2).linked should be (empty)
    updated.get(4)(3).linked should be (empty)
    updated.get(4)(4).linked should not be (empty)
    module.linked(updated.get(0)(0), updated.get(0)(1), bidi=true) should be (true)
    module.linked(updated.get(0)(1), updated.get(0)(2), bidi=true) should be (false)
    module.linked(updated.get(0)(2), updated.get(0)(3), bidi=true) should be (false)
    module.linked(updated.get(0)(3), updated.get(0)(4), bidi=true) should be (false)
    module.linked(updated.get(1)(0), updated.get(1)(1), bidi=true) should be (false)
    module.linked(updated.get(1)(1), updated.get(1)(2), bidi=true) should be (true)
    module.linked(updated.get(1)(2), updated.get(1)(3), bidi=true) should be (false)
    module.linked(updated.get(1)(3), updated.get(1)(4), bidi=true) should be (false)
    module.linked(updated.get(2)(0), updated.get(2)(1), bidi=true) should be (false)
    module.linked(updated.get(2)(1), updated.get(2)(2), bidi=true) should be (false)
    module.linked(updated.get(2)(2), updated.get(2)(3), bidi=true) should be (true)
    module.linked(updated.get(2)(3), updated.get(2)(4), bidi=true) should be (false)
    module.linked(updated.get(3)(0), updated.get(3)(1), bidi=true) should be (false)
    module.linked(updated.get(3)(1), updated.get(3)(2), bidi=true) should be (false)
    module.linked(updated.get(3)(2), updated.get(3)(3), bidi=true) should be (false)
    module.linked(updated.get(3)(3), updated.get(3)(4), bidi=true) should be (true)
    module.linked(updated.get(4)(0), updated.get(4)(1), bidi=true) should be (false)
    module.linked(updated.get(4)(1), updated.get(4)(2), bidi=true) should be (false)
    module.linked(updated.get(4)(2), updated.get(4)(3), bidi=true) should be (false)
    module.linked(updated.get(4)(3), updated.get(4)(4), bidi=true) should be (false)
    module.linked(updated.get(0)(1), updated.get(1)(1), bidi=true) should be (true)
    module.linked(updated.get(1)(2), updated.get(2)(2), bidi=true) should be (true)
    module.linked(updated.get(2)(3), updated.get(3)(3), bidi=true) should be (true)
    module.linked(updated.get(3)(4), updated.get(4)(4), bidi=true) should be (true)
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
    leftOFBottomRightCell = leftOFBottomRightCell.copy(linked = Set(Coordinates(4, 4)))
    grid = grid.set(leftOFBottomRightCell)
    grid = grid.set(bottomRightCell) 
    // info(grid.toString())
    When("determining whether bottom right corner cell is reachable from the cell to its left")
    Then("these 2 cells are reachable from each other")
    module.reachable(grid, 4, 3, 4, 4) shouldBe (true)
    When("determining whether bottom right corner cell is reachable from the upper left corner cell")
    Then("these 2 cells are unreachable from each other")
    module.reachable(grid, 0, 0, 4, 3) shouldBe (false)
    // When("deisolating cells in the grid")
    // grid = sidewinder.deisolateCells(grid)
    Then("bottom right corner cell is still reachable from the cell to its left")
    module.reachable(grid, 4, 3, 4, 4) shouldBe (true)
    Then("bottom right corner cell should now be reachable from upper left corner cell")
    module.reachable(grid, 0, 0, 4, 4) shouldBe (true)
  }

  it should "know when lower-right corner cell is unreachable (e.g. not linked to uppr-left corner cell)" in {
    case object linker extends Linkage
    
    case object binaryTree extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x5 grid with a completely isolated (e.g. isolated from all cells) bottom-right corner cell") 
    var grid: Grid = binaryTree.generate(5, 5)
    var bottomRightCell: Cell = grid.get(4)(4)
    for (linked <- bottomRightCell.linked) {
      val linkedCell: Cell = grid.get(linked.x)(linked.y)
      val unlinked: Cell = linkedCell.copy(linked = linkedCell.linked.filter(c => c != linkedCell.coords))
      grid = grid.set(unlinked)
    }
    grid = grid.set(bottomRightCell.copy(linked = Set()))
    def unlinkUnwantedCells(cell: Cell, unwanted: Seq[Cell]): Cell = {
      cell.copy(linked = cell.linked.filter(c => unwanted.count(c2 => c2.coords == c) == 0))
    }
    val unreachables: Seq[Cell] = Seq(bottomRightCell)
    var cell: Cell = grid.get(4)(3)
    cell = unlinkUnwantedCells(cell, unreachables)
    grid = grid.set(cell)
    cell = grid.get(3)(4)
    grid = grid.set(cell)
    cell = unlinkUnwantedCells(cell, unreachables)
    grid = grid.set(cell)
    bottomRightCell = grid.get(bottomRightCell.coords.x)(bottomRightCell.coords.y)
    // info(grid.toString)
    module.reachable(grid, 0, 0, 4, 4) shouldBe (false)
    binaryTree.distance.distances(grid, 4, 4).toSeq should have length (1)
    info(binaryTree.distance.distances(grid, 4, 4).get(Coordinates(4, 4)).getOrElse("").toString())
    binaryTree.distance.distances(grid, 0, 0).keySet should not contain (Coordinates(4, 4))
  }

  it should "know when lower-right corner cell is linked to upper-left corner cell via other cells" in {
    case object linker extends Linkage
    
    case object binaryTree extends BinaryTree {
      case object _linkage extends Linkage
      override type LINKAGE = Linkage
      override val linker = _linkage
      case object _distance extends Distance
      override type DISTANCE = Distance
      override val distance = _distance
    }
    Given("5x5 grid with a completely isolated (e.g. isolated from all cells) bottom-right corner cell") 
    var grid: Grid = binaryTree.generate(5, 5)
    var bottomRightCell: Cell = grid.get(4)(4)
    for (linked <- bottomRightCell.linked) {
      val linkedCell: Cell = grid.get(linked.x)(linked.y)
      val unlinked: Cell = linkedCell.copy(linked = linkedCell.linked.filter(c => c != linkedCell.coords))
      grid = grid.set(unlinked)
    }
    grid = grid.set(bottomRightCell.copy(linked = Set(Coordinates(4, 3))))
    bottomRightCell = grid.get(bottomRightCell.coords.x)(bottomRightCell.coords.y)
    module.reachable(grid, 0, 0, 4, 4) shouldBe (true)
  }

}