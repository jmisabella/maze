package maze.behaviors.solvers

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.Sidewinder
import maze.behaviors.solvers.Dijkstra
import maze.classes.{ Cell, Grid, Coordinates }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class DijkstraSpec extends AnyFlatSpec with GivenWhenThen {
  case object dijkstra extends Dijkstra {
    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }
  case object sidewinder extends Sidewinder {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage
    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }

  "Dijkstra" should "generate a 20x20 maze using Sidewinder and print its solution steps from upper-right corner to lower-left corner" in {
    Given("a 20x20 grid generated using Sidewinder")
    val generated: Grid = sidewinder.generate(20, 20)
    Then("generated maze should have height of 20 cells")
    generated.rows should be (20)
    generated.cells.length should be (20)
    Then("generated maze should have width of 0 cells")
    generated.columns should be (20)
    generated.cells.count(c => c.length == 20) should be (20)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("20x20 Sidewinder")
    println(generated)
    When("solving maze using Dijkstra's algorithm")
    val solved: Grid = dijkstra.solve(generated, 0, 19, 19, 0)
    Then("the solution should be printed to screen")
    println(solved)
  }

  it should "generate a 20x20 maze using Sidewinder and print its solution in letter Xs from upper-right corner to lower-left corner" in {
    Given("a 20x20 grid generated using Sidewinder")
    val generated: Grid = sidewinder.generate(20, 20)
    Then("generated maze should have height of 20 cells")
    generated.rows should be (20)
    generated.cells.length should be (20)
    Then("generated maze should have width of 0 cells")
    generated.columns should be (20)
    generated.cells.count(c => c.length == 20) should be (20)
    When("printing the maze")
    Then("the maze should be printed to screen")
    println("20x20 Sidewinder")
    println(generated)
    When("solving maze using Dijkstra's algorithm")
    val solved: Grid = dijkstra.solve(generated, 0, 19, 19, 0, Some('X'))
    Then("the solution should be printed to screen")
    println(solved)
  }
}