package triangle

import maze.behaviors.builders.Generator
import maze.classes.{ Cell, Coordinates, MazeRequest }
import maze.classes.CellOrientation._
import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.direction.TriangleDirection._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.{ Polygon, Line }
import scalafx.scene.text.{ Font, Text }
import maze.behaviors.builders.RecursiveBacktracker
import java.util.Arrays

case class Triangle(
  v1: (Double, Double),
  v2: (Double, Double),
  v3: (Double, Double),
  orientation: CellOrientation,
  walls: TriangleWalls,
  coords: Option[Coordinates] = None
) {
  def points: Array[Double] = Array(v1._1, v1._2, v2._1, v2._2, v3._1, v3._2)

  // Convert the triangle points to a polygon
  def toPolygon: Polygon = {
    val polygon = new Polygon()
    polygon.getPoints.addAll(Arrays.asList(points.map(java.lang.Double.valueOf): _*))
    polygon.fill = Color.Transparent
    polygon.stroke = Color.Transparent
    polygon
  }

  // Render walls individually based on the wall states
  def renderWalls: Seq[Line] = {
    orientation match {
      case Normal =>
        Seq(
          if (walls.upperLeft) Some(Line(v1._1, v1._2, v2._1, v2._2)) else None,
          if (walls.upperRight) Some(Line(v2._1, v2._2, v3._1, v3._2)) else None,
          if (walls.bottom) Some(Line(v3._1, v3._2, v1._1, v1._2)) else None
        ).flatten

      case Inverted =>
        Seq(
          if (walls.top) Some(Line(v1._1, v1._2, v2._1, v2._2)) else None,
          if (walls.lowerLeft) Some(Line(v2._1, v2._2, v3._1, v3._2)) else None,
          if (walls.lowerRight) Some(Line(v3._1, v3._2, v1._1, v1._2)) else None
        ).flatten
    }
  }

  // Render optional coordinates as text
  def renderCoordinates: Option[Text] = coords.map { coord =>
    new Text(v1._1 + (v3._1 - v1._1) / 2, v1._2 + (v3._2 - v1._2) / 2, s"${coord.x}, ${coord.y}") {
      fill = Color.Gray
      font = Font(10)
    }
  }
}

case class TriangleWalls(
  upperLeft: Boolean,
  upperRight: Boolean,
  bottom: Boolean,
  top: Boolean,
  lowerLeft: Boolean,
  lowerRight: Boolean
)
object TriangleWalls {
  def apply(cell: Cell): TriangleWalls = {
    TriangleWalls(
      upperLeft = !cell.isLinked(UpperLeft),
      upperRight = !cell.isLinked(UpperRight),
      bottom = !cell.isLinked(Down),
      top = !cell.isLinked(Up),
      lowerLeft = !cell.isLinked(LowerLeft),
      lowerRight = !cell.isLinked(LowerRight)
    )
  }
}

object TriangleGridApp2 extends JFXApp {
  val rows = 16
  val cols = 20
  // val cellSize = 35
  val cellSize = 50 
  val triangleHeight = Math.sqrt(3) / 2 * cellSize

  // Create the maze
  val request = MazeRequest(Delta, cols, rows, HuntAndKill, Coordinates(0, 0), Coordinates(2, 1))
  val fullMaze = Generator.generate(request)
  val maze = fullMaze.cells

  stage = new JFXApp.PrimaryStage {
    title = "Triangle Maze"
    scene = new Scene(cols * cellSize / 2, rows * triangleHeight) {
      val shapes = for {
        row <- maze.indices
        col <- maze(row).indices
        cell = maze(row)(col)

        x = col * cellSize
        y = row * triangleHeight / 2

        val adjustedHeightFactor: Double = 2.0
        val adjustedWidthFactor: Double = 0.5

        // Create the triangle with walls based on the cell's orientation
        triangle = cell.orientation match {
          case Normal => Triangle(
            (x * adjustedWidthFactor + cellSize / 2, y * adjustedHeightFactor),
            (x * adjustedWidthFactor, y * adjustedHeightFactor + triangleHeight),
            (x * adjustedWidthFactor + cellSize, y * adjustedHeightFactor + triangleHeight),
            Normal,
            TriangleWalls(cell),
            Some(cell.coords)
          )
          case Inverted => Triangle(
            (x * adjustedWidthFactor, y * adjustedHeightFactor),
            (x * adjustedWidthFactor + cellSize, y * adjustedHeightFactor),
            (x * adjustedWidthFactor + cellSize / 2, y * adjustedHeightFactor + triangleHeight),
            Inverted,
            TriangleWalls(cell),
            Some(cell.coords)
          )
        }

        polygon = triangle.toPolygon
        walls = triangle.renderWalls
      } yield Seq(polygon) ++ walls ++ triangle.renderCoordinates.toSeq

      content = shapes.flatten
    }
  }
}
