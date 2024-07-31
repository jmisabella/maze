package maze.behaviors.builders

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.Wilsons
import maze.classes.{ Cell, Grid, Coordinates }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class WilsonsSpec extends AnyFlatSpec with GivenWhenThen {
  case object module extends Wilsons {
    case object _linkage extends Linkage
    override type LINKAGE = Linkage
    override val linker = _linkage

    case object _distance extends Distance
    override type DISTANCE = Distance
    override val distance = _distance
  }
  
  // "Wilsons" should "generate a 5x5 maze using Binary Tree and print it to screen" in {
  //   Given("5x5 grid")
  //   val grid = Grid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
  //   When("generating maze using Wilsons")
  //   val generated: Grid = module.generate(grid)
  //   Then("generated maze should have height of 5 cells")
  //   generated.rows should be (5)
  //   generated.cells.length should be (5)
  //   Then("generated maze should have width of 5 cells")
  //   generated.columns should be (5)
  //   generated.cells.count(c => c.length == 5) should be (5)
  //   When("printing the maze")
  //   Then("the maze should be printed to screen")
  //   println(generated.toString())
  //   println(generated.asci())
  // }

}
