package maze.classes

import maze.classes.{ Cell, Grid }

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

  it should "update cell to be visited" in {
    Given("3x3 grid with all unvisited cells") 
    val grid = Grid(3, 3)
    grid.count(c => !c.visited) should be (9)
    When("updating first cell to be visited")
    val first: Cell = grid.get(0)(0).copy(visited = true)
    val updatedGrid: Grid = grid.set(first)
    Then("first cell in grid is only cell in grid which has been visited")
    updatedGrid.get(0)(0).visited should be (true)
    updatedGrid.count(c => c.visited) should be (1)
    updatedGrid.count(c => !c.visited) should be (8)
  }

  it should "flatten and unflatten its grid to end up with original X by Y matrix" in {
    Given("5x5 grid with all unvisited cells") 
    val grid: Grid = Grid(5, 5)
    When("flattening grid into a list")
    val flattened = grid.flatten()
    Then("flattened list's length should equal original grid's rows multiplied by columns")
    flattened.length should equal (grid.rows * grid.columns)
    When("unflattening back to a grid")
    val unflattened: Grid = grid.unflatten(flattened)
    Then("the unflattened grid should equal the original grid")
    grid.rows should equal (unflattened.rows)
    grid.columns should equal (unflattened.columns)
    grid.cells should equal (unflattened.cells)
    When("updating middle cell to be visited and flattening and re-unflattening cells")
    val middle: Cell = unflattened.get(2)(2).visit()
    middle.visited should be (true)
    val grid2 = unflattened.set(middle)
    grid2.get(2)(2).visited should be (true)
    val unflattened2: Grid = grid2.unflatten(grid2.flatten())
    Then("the unflattend grid should equal the original, such that only the middle cell has been visited")
    grid2.rows should equal (unflattened2.rows)
    grid2.columns should equal (unflattened2.columns)
    grid2.cells should equal (unflattened2.cells)
  }

  it should "know which cells have been linked together" in {
    Given("4x4 grid") 
    val grid: Grid = Grid(4, 4)
    When("linking all cells in bottom row together")
    // TODO: move linking functionality into a Linkage behavior
    val bottomRowWithDupes: List[Cell] = (for ((c1, c2) <- grid.row(3) zip grid.row(3).drop(1)) yield c1.link(c2)).flatten
    val grouped = bottomRowWithDupes.groupBy(c => (c.coords, c.visited, c.neighbors))
    // val merged = grouped.foldLeft(Map.empty[(Coordinates, Boolean, Neighbors), List[Cell]].withDefaultValue(Nil)) {
    val merged = grouped.foldLeft(Nil: Seq[Option[Cell]]) {
      case (acc, (k, v)) => {
        val coords: Coordinates = k._1
        val visited: Boolean = k._2
        val neighbors: Neighbors = k._3
        val linked: Set[Coordinates] = v.map(c => c.linked).toSet.flatten
        // acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked)))
        acc ++ Seq(Some(Cell(coords = coords, visited = visited, neighbors = neighbors, linked = linked)))
      }
    }
    // println("##########################################")
    // println(grouped.getClass)
    // println(grouped.mkString("\n--------------------------\n"))
    // println("##########################################")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println("~~~~~~~~~~~~~~~~~~~~~ UPDATED ~~~~~~~~~~~~~~~~~~~~~")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(merged.mkString("\n"))
    // var updated: Grid = grid
    // for (cell <- bottomRow) {
    //   updated = updated.set(cell)
    // }
    // println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    // println("~~~~~~~~~~~~~~~~~~~~~ UPDATED ~~~~~~~~~~~~~~~~~~~~~")
    // println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    // for (row <- updated.cells) {
    //   for (cell <- row) {
    //     println(cell)
    //   }
    // }

  }

}