package maze.behaviors

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ Generator, BinaryTree, Sidewinder }
import maze.classes.{ Cell, Grid, MazeRequest, Algorithm }
// import maze.classes.cell.SquareCell
// import maze.classes.grid.SquareGrid
import maze.classes.MazeType._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen
import maze.classes.Coordinates

class GeneratorSpec extends AnyFlatSpec with GivenWhenThen {
  "Generator" should "generate a 5x5 maze using BinaryTree algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(Orthogonal, 5, 5, Algorithm.BinaryTree, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    val grid = Generator.gen(request)
    // val grid = Generator.generate(request)
    Then("grid should be generated as 5x5")
    grid.height should be (5)
    grid.width should be (5)
    println(grid.toString())
    println(grid.asci()) 
  }

  it should "generate a 14x20 maze using Sidewinder algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(Orthogonal, 14, 20, Algorithm.Sidewinder, Coordinates(0, 19), Coordinates(13, 0))
    When("generating the grid")
    // val grid = Generator.generate(request)
    val grid = Generator.gen(request)
    Then("grid should be generated as 14x20")
    grid.height should be (20)
    grid.width should be (14)
    println(grid.toString())
    println(grid.asci())
  }

  it should "generate a 5x10 maze using Sidewinder algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(Orthogonal, 5, 10, Algorithm.Sidewinder, Coordinates(0, 9), Coordinates(4, 0))
    When("generating the grid")
    val grid = Generator.generate(request)
    Then("grid should be generated as 5x10")
    grid.height should be (10)
    grid.width should be (5)
    println(grid.toString())
    println("ATTENTION")
    println(grid.asci())
  }

  it should "generate a solved 5x5 maze using BinaryTree algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(Orthogonal, 5, 5, Algorithm.BinaryTree, Coordinates(0, 4), Coordinates(4, 0))
    When("generating the grid")
    val grid = Generator.generate(request)
    Then("grid should be generated as 5x5")
    grid.height should be (5)
    grid.width should be (5)
    println(grid.toString())
    println(grid.asci()) 
  }

}