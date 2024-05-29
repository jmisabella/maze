package maze.behaviors.builders

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.Sidewinder
import maze.classes.{ Cell, Grid, Coordinates }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class SidewinderSpec extends AnyFlatSpec with GivenWhenThen {

  case object module extends Sidewinder {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage
    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }
  
  "Sidewinder" should "generate a 5x5 maze using Sidewinder and print it to screen" in {
    Given("5x5 grid")
    val grid = Grid(5, 5)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 5 cells")
    generated.rows should be (5)
    generated.cells.length should be (5)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (5)
    generated.cells.count(c => c.length == 5) should be (5)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("5x5 Sidewinder")
    println(generated.toString())
  }

  it should "generate a 12x12 maze using Sidewinder and print it to screen" in {
    Given("12x12 grid")
    val grid = Grid(12, 12)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 12 cells")
    generated.rows should be (12)
    generated.cells.length should be (12)
    Then("generated maze should have width of 12 cells")
    generated.columns should be (12)
    generated.cells.count(c => c.length == 12) should be (12)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("12x12 Sidewinder")
    println(generated.toString())
  }


  it should "generate a 25x25 maze using Sidewinder and print it to screen" in {
    Given("25x25 grid")
    val grid = Grid(25, 25)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 25 cells")
    generated.rows should be (25)
    generated.cells.length should be (25)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (25)
    generated.cells.count(c => c.length == 25) should be (25)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("25x25 Sidewinder")
    println(generated.toString())
  }

  it should "generate a 35x35 maze using Sidewinder and print it to screen" in {
    Given("35x35 grid")
    val grid = Grid(35, 35)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 35 cells")
    generated.rows should be (35)
    generated.cells.length should be (35)
    Then("generated maze should have width of 5 cells")
    generated.columns should be (35)
    generated.cells.count(c => c.length == 35) should be (35)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("35x35 Sidewinder")
    println(generated.toString())
    println(module.distance.showPathTo(generated, 0, 34, 34, 0))
  }

  it should "generate a 40x40 maze using Sidewinder and print it to screen" in {
    Given("40x40 grid")
    val grid = Grid(40, 40)
    When("generating maze using Sidewinder")
    val generated: Grid = module.generate(grid)
    Then("generated maze should have height of 40 cells")
    generated.rows should be (40)
    generated.cells.length should be (40)
    Then("generated maze should have width of 40 cells")
    generated.columns should be (40)
    generated.cells.count(c => c.length == 40) should be (40)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("40x40 Sidewinder")
    println(generated.toString())
    println(module.distance.showPathTo(generated, 0, 39, 39, 0))
  }
  
  it should "generate a maze in which each cell is accessible from the upper-left corner cell" in {
    When("generating 5x5 maze using Sidewinder")
    val grid: Grid = module.generate(5, 5)
    Then("generated maze should have height of 5 cells")
    grid.rows should be (5)
    grid.cells.length should be (5)
    Then("generated maze should have width of 5 cells")
    grid.columns should be (5)
    grid.cells.count(c => c.length == 5) should be (5)
    Then("each cell should be accessible from the upper-left cell")
    val dist = module.distance.distances(grid, 0, 0)
    for (cell <- grid) {
      dist.keySet should contain (cell.coords) 
      println(s"COORDS: ${cell.coords.x}, ${cell.coords.y}")
      println(s"DIST: ${dist.get(cell.coords)}")
    }
    println(module.distance.showDistances(grid, 0, 0))
  }
  

}
