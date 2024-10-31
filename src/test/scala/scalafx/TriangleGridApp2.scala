package triangle

import maze.classes.{ Cell, Grid, Coordinates, CellOrientation }
import maze.classes.CellOrientation._
import maze.classes.MazeType._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Polygon
import java.util.Arrays
import play.api.libs.json.{ Json, Format }

// object TriangleType extends Enumeration {
//   type TriangleType = Value
//   val Upward, Downward = Value
  
//   implicit val format: Format[TriangleType] = Json.formatEnum(this)

//   def fromString(s: String): Option[TriangleType] = values.find(_.toString.toLowerCase == s.toLowerCase())
// }
// import TriangleType._

case class Triangle(v1: (Double, Double), v2: (Double, Double), v3: (Double, Double), orientation: CellOrientation) {
  def walls: Walls = Walls(orientation) 
  def points: Array[Double] = Array(v1._1, v1._2, v2._1, v2._2, v3._1, v3._2)
  def toPolygon: Polygon = {
    val polygon = new Polygon()
    // Convert Scala Double to Java Double
    polygon.getPoints.addAll(Arrays.asList(points.map(java.lang.Double.valueOf): _*))
    polygon.fill = Color.Transparent
    polygon.stroke = Color.Black
    polygon
  }
}

// // Walls class to manage the wall state
case class Walls(upperLeft: Boolean, upperRight: Boolean, bottom: Boolean, top: Boolean, lowerLeft: Boolean, lowerRight: Boolean)
object Walls {
  def apply(orientation: CellOrientation): Walls = orientation match {
    case Normal => Walls(true, true, true, false, false, false)
    case Inverted => Walls(false, false, false, true, true, true)
  }
}

// // Cell class
// case class Cell(row: Int, col: Int, triangleType: TriangleType) {
//   var walls: Walls = Walls(triangleType)
// }

object TriangleGridApp2 extends JFXApp {
  val rows = 16 
  val cols = 20 
  val cellSize = 35 
  val triangleHeight = Math.sqrt(3) / 2 * cellSize

  // Create the maze
  val maze = createMaze(rows, cols)

  def createMaze(rows: Int, cols: Int): Array[Array[Cell]] = {
    val maze = Array.ofDim[Cell](rows, cols)
    for (row <- 0 until rows; col <- 0 until cols) {
      // Alternating between upward and downward triangles
      val triangleType = if ((row + col) % 2 == 0) Normal else Inverted
      maze(row)(col) = Cell(Delta, Coordinates(row, col), triangleType)
    }
    maze
  }

  stage = new JFXApp.PrimaryStage {
    title = "Triangle Maze"
    // scene = new Scene(cols * cellSize, rows * triangleHeight) {
    scene = new Scene(cols * cellSize / 2, rows * triangleHeight) {
      val shapes = for {
        row <- maze.indices
        col <- maze(row).indices
        cell = maze(row)(col)

        // Adjust x for odd rows (stagger the columns)
        x = col * cellSize
        y = row * triangleHeight / 2 // Adjust y for triangle height

        // Define upward or downward triangles based on cell type
        val adjustedHeightFactor: Double = 2.0
        val adjustedWidthFactor: Double = 0.5

        triangle = cell.orientation match {
          case Normal => Triangle(
            (x * adjustedWidthFactor + cellSize / 2, y * adjustedHeightFactor),               // Top
            (x * adjustedWidthFactor, y * adjustedHeightFactor + triangleHeight),             // Bottom Left
            (x * adjustedWidthFactor + cellSize, y * adjustedHeightFactor + triangleHeight),   // Bottom Right
            Normal 
          )
          case Inverted => Triangle(
            (x * adjustedWidthFactor, y * adjustedHeightFactor),                             // Top Left
            (x * adjustedWidthFactor + cellSize, y * adjustedHeightFactor),                   // Top Right
            (x * adjustedWidthFactor + cellSize / 2, y * adjustedHeightFactor + triangleHeight), // Bottom
            Inverted 
          )
        }

        polygon = triangle.toPolygon
      } yield polygon

      content = shapes
    }
  }
}
