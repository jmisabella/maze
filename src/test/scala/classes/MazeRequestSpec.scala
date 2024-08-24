package maze.classes

import maze.classes.{ MazeRequest, Coordinates }
import maze.classes.Algorithm._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class MazeRequestSpec extends AnyFlatSpec with GivenWhenThen {
  private val left = Symbol("left")
  private val right = Symbol("right")

  "MazeRequest" should "create valid JSON from 5x5 BinaryTree request before deserializing it back to the original request" in {
    Given("5x5 BinaryTree maze request")
    val request = MazeRequest(5, 5, BinaryTree, Coordinates(0, 4), Coordinates(4, 0))
    When("serializing it into a JSON string")
    val json = request.toString()
    Then("resulting JSON should be valid JSON and deserialize back into an identical request")
    info(s"resulting JSON: $json") 
    val deserialized: MazeRequest = MazeRequest(json)
    deserialized should equal (request)
  }

  it should "create valid JSON from 8x8 Sidewinder request before deserializing it back to the original request" in {
    Given("8x8 Sidewinder maze request")
    val request = MazeRequest(8, 8, Sidewinder, Coordinates(0, 7), Coordinates(7, 0))
    When("serializing it into a JSON string")
    val json = request.toString()
    Then("resulting JSON should be valid JSON and deserialize back into an identical request")
    info(s"resulting JSON: $json") 
    val deserialized: MazeRequest = MazeRequest(json)
    deserialized should equal (request)
  }

  it should "deserialize a JSON 8x8 Sidewinder maze request which is valid JSON but each argument's type is a String" in {
    Given("string representation of 8x8 sidewinder maze request whose arguments are all of type String")
    val json = """{"width":"8","height":"8","algorithm":"sidewinder","startX":"0","startY":"7","goalX":"7","goalY":"0"}"""
    When(s"deserializing the string $json")
    val request: MazeRequest = MazeRequest(json)
    Then("a MazeRequest object should be created")
    request should equal (MazeRequest(8, 8, Sidewinder, Coordinates(0,7), Coordinates(7,0)))
  }

}