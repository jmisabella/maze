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
    val unlinked = Grid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    val grid: Grid = sidewinder.generate(unlinked)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.distances(grid, 0, 0)
    println(result)
  }

  it should "generate a 5x5 maze using Sidewinder and show shortest path from upper-left cell to botom-right cell before printing to screen" in {
    Given("5x5 grid generated using Sidewinder")
    val unlinked = Grid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    val grid: Grid = sidewinder.generate(unlinked)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.pathTo(grid, 0, 0, 4, 4)
    println(result)
  }

  it should "generate a 5x5 maze using Sidewinder and determine shortest path from upper-left cell to botom-right cell" in {
    Given("5x5 grid generated using Sidewinder")
    val grid: Grid = sidewinder.generate(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    println(grid)
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.getPathTo(grid, 0, 0, 4, 4)
    info(sidewinder.distance.pathTo(grid, 0, 0, 4, 4).toString)
    val cellCountOnShortestPath: Int = grid.count(c => result.keySet.contains(c.coords))
    cellCountOnShortestPath shouldBe > (0)
    cellCountOnShortestPath shouldBe >= (4 + 4)
  }
  
  it should "generate a 5x5 maze using Sidewinder and determine longest path" in {
    Given("5x5 grid generated using Sidewinder")
    val grid: Grid = sidewinder.generate(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    println(grid)
    When("determining longest path")
    val result = sidewinder.distance.getLongestPath(grid)
    info(sidewinder.distance.longestPath(grid).toString)
    val cellCountOnShortestPath: Int = grid.count(c => result.keySet.contains(c.coords))
    cellCountOnShortestPath shouldBe > (0)
    cellCountOnShortestPath shouldBe >= (4 + 4)
  }
  
  it should "generate a 5x5 maze using Sidewinder and determine longest path but print in overriding o character" in {
    Given("5x5 grid generated using Sidewinder")
    val grid: Grid = sidewinder.generate(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    println(grid)
    When("determining longest path")
    val result = sidewinder.distance.getLongestPath(grid)
    info(sidewinder.distance.longestPath(grid, Some('o')).toString() )
    val cellCountOnShortestPath: Int = grid.count(c => result.keySet.contains(c.coords))
    cellCountOnShortestPath shouldBe > (0)
    cellCountOnShortestPath shouldBe >= (4 + 4)
  }

  it should "generate a 5x5 maze using Binary Tree and determine distances from upper-left cell to all other reachable cells before printing to screen" in {
    Given("5x5 grid generated using BinaryTree")
    val unlinked = Grid(5, 5, Coordinates(0, 4), Coordinates(4, 0))
    val grid: Grid = binaryTree.generate(unlinked)
    println(grid)
    println(grid.asci())
    When("determining distances from upper-left cell to each other cell")
    val result = sidewinder.distance.distances(grid, 0, 0)
    Then("exactly 1 cell has distance of 0") 
    result.count(c => c.distance == 0) should equal (1)
    info(result.asci())
    info(result.toString())
  }



}
