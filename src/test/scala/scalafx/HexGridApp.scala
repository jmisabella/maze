package hex

import maze.behaviors.builders.Generator
import maze.classes.{ Cell, Grid, Coordinates, MazeRequest }
import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.direction.HexDirection._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.{ Polygon, Line }
import scalafx.scene.text.Text
import java.util.Arrays

// Hexagon representation
case class Hexagon(centerX: Double, centerY: Double, size: Double, walls: HexWalls, coords: Option[Coordinates] = None) {
  // Generate a polygon shape for the hexagon without walls (walls will be drawn separately)
  def toPolygon: Polygon = {
    val points = for (i <- 0 until 6) yield {
      val angle = Math.PI / 3 * i
      val x = centerX + size * Math.cos(angle)
      val y = centerY + size * Math.sin(angle)
      Array(x, y)
    }
    val polygon = new Polygon()
    polygon.getPoints.addAll(Arrays.asList(points.flatten.map(java.lang.Double.valueOf): _*))
    polygon.fill = Color.Transparent
    polygon.stroke = Color.Transparent // Make hexagon itself transparent; walls are drawn separately
    polygon
  }

  // Render walls individually based on wall states
  def renderWalls: Seq[Line] = {
    val points = (0 until 6).map { i =>
      val angle = Math.PI / 3 * i
      (centerX + size * Math.cos(angle), centerY + size * Math.sin(angle))
    }
    Seq(
      if (walls.southeast) Some(Line(points(0)._1, points(0)._2, points(1)._1, points(1)._2)) else None,
      if (walls.south) Some(Line(points(1)._1, points(1)._2, points(2)._1, points(2)._2)) else None,
      if (walls.southwest) Some(Line(points(2)._1, points(2)._2, points(3)._1, points(3)._2)) else None,
      if (walls.northwest) Some(Line(points(3)._1, points(3)._2, points(4)._1, points(4)._2)) else None,
      if (walls.north) Some(Line(points(4)._1, points(4)._2, points(5)._1, points(5)._2)) else None,
      if (walls.northeast) Some(Line(points(5)._1, points(5)._2, points(0)._1, points(0)._2)) else None
    ).flatten
    // Seq(
    //   // below line incorrectly renders north wall as northwest or southeast, line should be horizontal
    //   // if (walls.north) Some(Line(points(0)._1, points(0)._2, points(1)._1, points(1)._2)) else None,
    //   if (walls.southeast) Some(Line(points(0)._1, points(0)._2, points(1)._1, points(1)._2)) else None,
    //   // below line incorrectly renders northeast as a horizontal line as if it's north or south
    //   // if (walls.northeast) Some(Line(points(1)._1, points(1)._2, points(2)._1, points(2)._2)) else None,
    //   if (walls.south) Some(Line(points(1)._1, points(1)._2, points(2)._1, points(2)._2)) else None,
    //   // below line incorrectly renders southeast as if it's southwest or northeast
    //   // if (walls.southeast) Some(Line(points(2)._1, points(2)._2, points(3)._1, points(3)._2)) else None,
    //   if (walls.southwest) Some(Line(points(2)._1, points(2)._2, points(3)._1, points(3)._2)) else None,
    //   // below line incorrectly renders south as if it's northwest or southeast (but not a horizontal line) 
    //   // if (walls.south) Some(Line(points(3)._1, points(3)._2, points(4)._1, points(4)._2)) else None,
    //   if (walls.northwest) Some(Line(points(3)._1, points(3)._2, points(4)._1, points(4)._2)) else None,
    //   // below line incorrectly renders southwest as a horizontal line line north or south 
    //   // if (walls.southwest) Some(Line(points(4)._1, points(4)._2, points(5)._1, points(5)._2)) else None,
    //   if (walls.north) Some(Line(points(4)._1, points(4)._2, points(5)._1, points(5)._2)) else None,
    //   // below line incorrectly renders northwest as if it's southwest or northeast 
    //   // if (walls.northwest) Some(Line(points(5)._1, points(5)._2, points(0)._1, points(0)._2)) else None
    //   if (walls.northeast) Some(Line(points(5)._1, points(5)._2, points(0)._1, points(0)._2)) else None
    // ).flatten
  }

  // Render optional coordinates as text
  def renderCoordinates: Option[Text] = coords.map { coord =>
    new Text(centerX - size / 4, centerY, s"${coord.x}, ${coord.y}") {
      fill = Color.Gray
    }
  }
}

// Case class to represent the wall state of a hex cell
case class HexWalls(north: Boolean, northeast: Boolean, southeast: Boolean, south: Boolean, southwest: Boolean, northwest: Boolean)
object HexWalls {
  def apply(cell: Cell): HexWalls = {
    HexWalls(
      north = !cell.isLinked(North),
      northeast = !cell.isLinked(NorthEast),
      southeast = !cell.isLinked(SouthEast),
      south = !cell.isLinked(South),
      southwest = !cell.isLinked(SouthWest),
      northwest = !cell.isLinked(NorthWest)
      // //// TODO: temporarily hard-coding walls to debug
      // north = true,
      // northeast = true,
      // southeast = true,
      // south = true,
      // southwest = true,
      // northwest = true

    )
  }
}

object HexGridApp extends JFXApp {
  val cellSize = 35.0 // Size of the hexagon (distance from center to any corner)
  val hexHeight = Math.sqrt(3) * cellSize // Height of a hexagon
  val hexWidth = 2 * cellSize // Width of a hexagon
  val rows = 10 // Logical grid rows
  val cols = 10 // Logical grid columns

  // Generate maze using the Generator and store cells
  // val request = MazeRequest(Sigma, cols, rows, HuntAndKill, Coordinates(0, 0), Coordinates(cols - 4, rows - 4))
  val request = MazeRequest(Sigma, cols, rows, AldousBroder, Coordinates(0, 0), Coordinates(cols - 4, rows - 4))
  val mazeGrid = Generator.generate(request)
  val maze = mazeGrid.cells
  println(mazeGrid)

  // Convert logical row, col to pixel-based center of the hexagon
  def hexCenter(row: Int, col: Int): (Double, Double) = {
    val x = col * 3.0 / 2.0 * cellSize // Horizontal distance between hex centers
    val y = row * hexHeight + (if (col % 2 == 1) hexHeight / 2 else 0) // Staggered rows for odd cols
    (x, y)
  }

  val hexagons = for {
        row <- maze.indices
        col <- maze(row).indices
        cell = maze(row)(col)
        (x, y) = hexCenter(row, col)
        // Hexagon with wall states based on cell links
        hexagon = Hexagon(x + cellSize, y + cellSize, cellSize, HexWalls(cell), Some(cell.coords))
      } yield hexagon

  stage = new JFXApp.PrimaryStage {
    title = "Hex Grid Maze with Coordinates"
    scene = new Scene(cols * hexWidth, rows * hexHeight) {
      // Render both hexagons and their walls, and add optional coordinates as text
      val shapes = hexagons.flatMap { hex =>
        Seq(hex.toPolygon) ++ hex.renderWalls ++ hex.renderCoordinates.toSeq
      }
      content = shapes
    }
  }

}