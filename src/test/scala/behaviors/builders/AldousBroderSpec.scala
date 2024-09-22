package maze.behaviors.builders

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.AldousBroder
import maze.classes.{ SquareCell, RectangleGrid, Coordinates }
import maze.classes.MazeType._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class AldousBroderSpec extends AnyFlatSpec with GivenWhenThen {
  case object module extends AldousBroder[SquareCell, RectangleGrid] {
    case object _linkage extends Linkage[SquareCell, RectangleGrid]
    override type LINKAGE = Linkage[SquareCell, RectangleGrid]
    override val linker = _linkage

    case object _distance extends Distance[SquareCell, RectangleGrid]
    override type DISTANCE = Distance[SquareCell, RectangleGrid]
    override val distance = _distance
  }
  
  "AldousBroder" should "generate a 5x5 maze using AldousBroder and print it to screen" in {
    Given("5x5 grid")
    val grid = RectangleGrid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    When("generating maze using AldousBroder")
    val generated = module.generate(grid)
    Then("generated maze should have height of 5 cells")
    generated.height should be (5)
    generated.cells.length should be (5)
    Then("generated maze should have width of 5 cells")
    generated.width should be (5)
    generated.cells.count(c => c.length == 5) should be (5)
    Then("resulting maze is a perfect maze")
    generated.isFullyConnected() shouldBe (true)
    generated.isPerfectMaze() shouldBe (true) 
    When("printing the maze")
    Then("the maze should be printed to screen")
    println(generated.toString())
    println(generated.asci())
    

  }

}

