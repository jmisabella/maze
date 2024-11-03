package triangle

import maze.behaviors.builders.Generator
import maze.classes.{ Cell, Grid, Coordinates, CellOrientation, MazeRequest }
import maze.classes.CellOrientation._
import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.direction.TriangleDirection._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.{ Polygon, Line }
import java.util.Arrays
import play.api.libs.json.{ Json, Format }
import maze.behaviors.builders.RecursiveBacktracker

case class Triangle(v1: (Double, Double), v2: (Double, Double), v3: (Double, Double), orientation: CellOrientation, walls: TriangleWalls) {
  def points: Array[Double] = Array(v1._1, v1._2, v2._1, v2._2, v3._1, v3._2)

  // Convert the triangle points to a polygon
  def toPolygon: Polygon = {
    val polygon = new Polygon()
    polygon.getPoints.addAll(Arrays.asList(points.map(java.lang.Double.valueOf): _*))
    polygon.fill = Color.Transparent
    polygon.stroke = Color.Transparent  // Make polygon transparent; walls are rendered separately
    polygon
  }

  // Render walls individually based on the wall states
  def renderWalls: Seq[Line] = {
    orientation match {
      case Normal =>
        Seq(
          if (walls.upperLeft) Some(Line(v1._1, v1._2, v2._1, v2._2)) else None, // Upper-left side
          if (walls.upperRight) Some(Line(v2._1, v2._2, v3._1, v3._2)) else None, // Upper-right side
          if (walls.bottom) Some(Line(v3._1, v3._2, v1._1, v1._2)) else None      // Bottom side
        ).flatten

      case Inverted =>
        Seq(
          if (walls.top) Some(Line(v1._1, v1._2, v2._1, v2._2)) else None,        // Top side
          if (walls.lowerLeft) Some(Line(v2._1, v2._2, v3._1, v3._2)) else None,  // Lower-left side
          if (walls.lowerRight) Some(Line(v3._1, v3._2, v1._1, v1._2)) else None  // Lower-right side
        ).flatten
    }
  }
}

case class TriangleWalls(upperLeft: Boolean, upperRight: Boolean, bottom: Boolean, top: Boolean, lowerLeft: Boolean, lowerRight: Boolean)
object TriangleWalls {
  def apply(orientation: CellOrientation): TriangleWalls = orientation match {
    case Normal => TriangleWalls(upperLeft = true, upperRight = true, bottom = true, top = false, lowerLeft = false, lowerRight = false)
    case Inverted => TriangleWalls(upperLeft = false, upperRight = false, bottom = false, top = true, lowerLeft = true, lowerRight = true)
  }
  def apply(cell: Cell): TriangleWalls = {
    val upperLeftWall: Boolean = !cell.isLinked(UpperLeft)
    val upperRightWall: Boolean = !cell.isLinked(UpperRight)
    val bottomWall: Boolean = !cell.isLinked(Down)
    val topWall: Boolean = !cell.isLinked(Up)
    val lowerLeftWall: Boolean = !cell.isLinked(LowerLeft)
    val lowerRightWall: Boolean = !cell.isLinked(LowerRight)
    TriangleWalls(upperLeftWall, upperRightWall, bottomWall, topWall, lowerLeftWall, lowerRightWall) 
  }
}

object TriangleGridApp2 extends JFXApp {
  val rows = 16 
  val cols = 20 
  val cellSize = 35 
  // val cellSize = 20 
  val triangleHeight = Math.sqrt(3) / 2 * cellSize

  // Create the maze
  // val maze = createMaze(rows, cols)
  // val request = MazeRequest(Delta, cols, rows, RecursiveBacktracker, Coordinates(0, 0), Coordinates(cols - 1, rows - 1))
  val request = MazeRequest(Delta, cols, rows, RecursiveBacktracker, Coordinates(0, 0), Coordinates(0, 0))
  val maze = Generator.generate(request).cells

  // def createMaze(rows: Int, cols: Int): Array[Array[Cell]] = {
  //   // val maze = Array.ofDim[Cell](rows, cols)
  //   // for (row <- 0 until rows; col <- 0 until cols) {
  //   //   val triangleType = if ((row + col) % 2 == 0) Normal else Inverted
  //   //   maze(row)(col) = Cell(Delta, Coordinates(row, col), triangleType)
  //   // }
  //   // maze
  //   val grid = Grid(Delta, rows, cols, Coordinates(0, 0), Coordinates(cols - 1, rows - 1))
  //   grid.cells
  // }

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
            (x * adjustedWidthFactor + cellSize / 2, y * adjustedHeightFactor),               // Top
            (x * adjustedWidthFactor, y * adjustedHeightFactor + triangleHeight),             // Bottom Left
            (x * adjustedWidthFactor + cellSize, y * adjustedHeightFactor + triangleHeight),   // Bottom Right
            Normal,
            // TriangleWalls(Normal)
            TriangleWalls(cell)
          )
          case Inverted => Triangle(
            (x * adjustedWidthFactor, y * adjustedHeightFactor),                             // Top Left
            (x * adjustedWidthFactor + cellSize, y * adjustedHeightFactor),                   // Top Right
            (x * adjustedWidthFactor + cellSize / 2, y * adjustedHeightFactor + triangleHeight), // Bottom
            Inverted,
            // TriangleWalls(Inverted)
            TriangleWalls(cell)
          )
        }

        polygon = triangle.toPolygon
        walls = triangle.renderWalls // Generate the wall lines
      } yield Seq(polygon) ++ walls

      content = shapes.flatten
    }
  }
}
