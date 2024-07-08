package maze.classes

import maze.classes.Coordinates

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.GivenWhenThen

class CoordinatesSpec extends AnyFlatSpec with GivenWhenThen {
  "Coordinates" should "deserialize from a JSON string" in {
    Given("JSON string indicating x and y as integers 0 and 0 respectfully")
    val json: String = """{"x":0,"y":0}"""
    When("deserializing JSON into a Coordinates object")
    val coords: Coordinates = Coordinates(json)
    Then("x should be 0")
    coords.x should equal (0)
    Then("y should be 0")
    coords.y should equal (0)
  }

  it should "deserialize from a JSON string representing a serialized Coordinates (e.g. types are all string)" in {
    Given("JSON string indicating x and y as integers 2 and 8 respectfully")
    val json: String = """{"x":"2","y":"8"}"""
    When("deserializing JSON into a Coordinates object")
    val coords: Coordinates = Coordinates(json)
    Then("x should be 0")
    coords.x should equal (2)
    Then("y should be 0")
    coords.y should equal (8)
  }
}