package maze.behaviors

import maze.classes.{ Cell, Grid }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class LinkageSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends Linkage

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

}