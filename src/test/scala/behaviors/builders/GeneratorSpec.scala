package maze.behaviors

import maze.behaviors.{ Linkage, Distance }
import maze.behaviors.builders.{ Generator, BinaryTree, Sidewinder }
import maze.classes.{ Cell, Grid, MazeRequest, Algorithm, MazeType }
import maze.classes.MazeType._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class GeneratorSpec extends AnyFlatSpec with GivenWhenThen {
  "Generator" should "generate a 5x5 maze using BinaryTree algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(5, 5, Algorithm.BinaryTree)
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    Then("grid should be generated as 5x5")
    grid.rows should be (5)
    grid.columns should be (5)
    println(grid.toString())
    println(grid.asci()) 
  }

  it should "generate a 14x20 maze using Sidewinder algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(14, 20, Algorithm.Sidewinder)
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    Then("grid should be generated as 14x20")
    grid.rows should be (20)
    grid.columns should be (14)
    println(grid.toString())
    println(grid.asci())
  }

  it should "generate a 5x10 maze using Sidewinder algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(5, 10, Algorithm.Sidewinder)
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    Then("grid should be generated as 5x10")
    grid.rows should be (10)
    grid.columns should be (5)
    println(grid.toString())
    println("ATTENTION")
    println(grid.asci())
  }

  it should "generate a solved 5x5 maze using BinaryTree algorithm from a request" in {
    Given("5x5 BinaryTree request")
    val request = MazeRequest(5, 5, Algorithm.BinaryTree, MazeType.Solved)
    When("generating the grid")
    val grid: Grid = Generator.generate(request)
    Then("grid should be generated as 5x5")
    grid.rows should be (5)
    grid.columns should be (5)
    println(grid.toString())
    println(grid.asci()) }
}