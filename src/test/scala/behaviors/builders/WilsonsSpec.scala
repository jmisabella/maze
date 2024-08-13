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
  
  "Wilsons" should "generate a 4x4 maze using Wilsons and print it to screen" in {
    // val dim: Int = 4 
    // val grid = Grid(dim, dim, Coordinates(0, dim - 1), Coordinates(dim - 1, 0))
    // val generated: Grid = module.generate(grid)
    // println(generated.toString())
    // println(generated.asci())
  }

}
