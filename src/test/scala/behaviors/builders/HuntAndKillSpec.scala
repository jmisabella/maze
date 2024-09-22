package maze.behaviors.builders

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.HuntAndKill
import maze.classes.{ SquareCell, RectangleGrid, Coordinates, MazeRequest, Algorithm }
import maze.classes.MazeType._
import maze.classes.Algorithm._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class HuntAndKillSpec extends AnyFlatSpec with GivenWhenThen {
  case object module extends HuntAndKill[SquareCell, RectangleGrid] {
    case object _linkage extends Linkage[SquareCell, RectangleGrid]
    override type LINKAGE = Linkage[SquareCell, RectangleGrid]
    override val linker = _linkage

    case object _distance extends Distance[SquareCell, RectangleGrid]
    override type DISTANCE = Distance[SquareCell, RectangleGrid]
    override val distance = _distance
  }
  
  "HuntAndKill" should "generate a 6x6 maze using HuntAndKill and print it to screen" in {
    Given("6x6 grid")
    val dim: Int = 6
    val grid = RectangleGrid(dim, dim, Coordinates(0, dim - 1), Coordinates(dim - 1, 0))
    When("generating maze using HuntAndKill algorithm")
    val generated = module.generate(grid)
    Then("resulting maze should contain no stranded unreachable cells") 
    generated.isFullyConnected() shouldBe (true)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    Then("resulting maze should be printed to screen using asci")
    info("\n" + generated.asci())
  }
  
  it should "generate a 30x30 maze using HuntAndKill and print it to screen" in {
    Given("30x30 grid")
    val dim: Int = 30
    val grid = RectangleGrid(dim, dim, Coordinates(0, dim - 1), Coordinates(dim - 1, 0))
    When("generating maze using HuntAndKill algorithm")
    val generated = module.generate(grid)
    Then("resulting maze should contain no stranded unreachable cells") 
    generated.isFullyConnected() shouldBe (true)
    Then("resulting maze is a perfect maze")
    generated.isPerfectMaze() shouldBe (true) 
    Then("resulting maze should be printed to screen using asci")
    info("\n" + generated.asci())
  }

  it should "honor start and goal coordinates specified in MazeRequest when generating a non-square HuntAndKill maze grid" in {
    Given("5x10 HuntAndKill request")
    val request = MazeRequest(Square, 5, 10, Algorithm.HuntAndKill, Coordinates(0, 9), Coordinates(4, 0))
    When("generating the grid")
    var grid = Generator.generate(request).asInstanceOf[RectangleGrid]
    Then("grid's start should be southwest")
    grid.startCoords should equal (Coordinates(0, request.height - 1))
    info("START COORDS: " + grid.startCoords.toString())
    Then("grid's goal should be northeast")
    grid.goalCoords should equal (Coordinates(request.width - 1, 0))
    Then("exactly one cell should be the goal cell (isGoal == true)")
    grid.count(c => c.isGoal) should equal (1)
    Then("exactly one cell should be the starting cell (isStart == true)")
    grid.count(c => c.isStart) should equal (1)
    Then("grid's start cell at 9,0 should have isStart set to true")
    grid.get(0, 9).isStart should be (true)
    Then("grid's 0,0 cell should have isStart set to false")
    grid.get(0, 0).isStart should be (false)
    Then("grid's goal cell at 0,4 should have isGoal set to true")
    grid.get(4, 0).isGoal should be (true)
    Then("grid's start cell at 9,0 should have isGoal set to false")
    grid.get(0, 9).isGoal should be (false)
    Then("resulting maze should contain no stranded unreachable cells") 
    grid.isFullyConnected() shouldBe (true)
    Then("resulting maze is a perfect maze")
    grid.isPerfectMaze() shouldBe (true) 
    Then("resulting maze should be printed to screen using asci")
    info("\n" + grid.asci())
  }

}
