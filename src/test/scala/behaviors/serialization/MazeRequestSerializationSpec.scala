package maze.behaviors.serialization

import maze.classes.{ Algorithm, MazeRequest }
import maze.classes.Algorithm._
import maze.behaviors.serialization.MazeRequestSerialization
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class MazeRequestSerializationSpec extends AnyFlatSpec with GivenWhenThen {
  case object module extends MazeRequestSerialization
  private val left = Symbol("left")
  private val right = Symbol("right")

  "MazeRequestSerialization" should "create valid JSON from 5x5 BinaryTree request before deserializing it back to the original request" in {
    Given("5x5 BinaryTree maze request")
    val request = MazeRequest(5, 5, BinaryTree)
    When("serializing it into a JSON string")
    val json1 = module.json(request)
    val json2 = request.toString()
    Then("resulting JSON should be valid JSON and deserialize back into an identical request")
    val deserialized1: Either[String, MazeRequest] = module.parse(json1)
    deserialized1 should be (right)
    deserialized1 should equal (Right(request))
    val deserialized2: Either[String, MazeRequest] = module.parse(json2)
    deserialized2 should be (right)
    deserialized2 should equal (Right(request))
  }

  it should "create valid JSON from 8x8 Sidewinder request before deserializing it back to the original request" in {
    Given("5x5 Sidewinder maze request")
    val request = MazeRequest(8, 8, Sidewinder)
    When("serializing it into a JSON string")
    val json1 = module.json(request)
    val json2 = request.toString()
    Then("resulting JSON should be valid JSON and deserialize back into an identical request")
    val deserialized1: Either[String, MazeRequest] = module.parse(json1)
    deserialized1 should be (right)
    deserialized1 should equal (Right(request))
    val deserialized2: Either[String, MazeRequest] = module.parse(json2)
    deserialized2 should be (right)
    deserialized2 should equal (Right(request))
  }

}