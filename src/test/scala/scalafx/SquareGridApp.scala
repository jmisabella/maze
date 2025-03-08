package square

import maze.behaviors.builders.Generator
import maze.classes.{ Cell, Coordinates, MazeRequest }
import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.direction.SquareDirection._
import maze.behaviors.builders.RecursiveBacktracker

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.{ Rectangle, Line }
import scalafx.scene.text.{ Font, Text }
import play.api.libs.json.{ Json, Format }

case class Square(x: Double, y: Double, size: Double, walls: SquareWalls, coords: Option[Coordinates] = None) {
  // Render the square shape itself
  def toRectangle: Rectangle = {
    val rect = new Rectangle {
      width = size
      height = size
      fill = Color.Transparent
      stroke = Color.Transparent
    }
    rect.relocate(x, y)
    rect
  }

  // Render walls individually based on the wall states
  def renderWalls: Seq[Line] = {
    Seq(
      if (walls.top) Some(Line(x, y, x + size, y)) else None,               // Top
      if (walls.right) Some(Line(x + size, y, x + size, y + size)) else None, // Right
      if (walls.bottom) Some(Line(x, y + size, x + size, y + size)) else None, // Bottom
      if (walls.left) Some(Line(x, y, x, y + size)) else None                // Left
    ).flatten
  }

  // Render optional coordinates as text
  def renderCoordinates: Option[Text] = coords.map { coord =>
    new Text(x + size / 2 - 10, y + size / 2 + 5, s"${coord.x}, ${coord.y}") {
      fill = Color.Gray
      font = Font(10)
    }
  }

}

// SquareWalls class to manage wall states for a square cell
case class SquareWalls(top: Boolean, right: Boolean, bottom: Boolean, left: Boolean)
object SquareWalls {
  def apply(cell: Cell): SquareWalls = {
    SquareWalls(
      top = !cell.isLinked(North),
      right = !cell.isLinked(East),
      bottom = !cell.isLinked(South),
      left = !cell.isLinked(West)
    )
  }
}

object SquareGridApp extends JFXApp {
  val rows = 16
  val cols = 20
  val cellSize = 35

  // Create the maze
//   val request = MazeRequest(Orthogonal, cols, rows, RecursiveBacktracker, Coordinates(0, 0), Coordinates(0, 0))
  val request = MazeRequest(Orthogonal, cols, rows, AldousBroder, Coordinates(0, 0), Coordinates(cols - 1, rows - 1))
  val maze = Generator.generate(request).cells

  val squares = for {
    row <- maze.indices
    col <- maze(row).indices
    cell = maze(row)(col)
    x = col * cellSize
    y = row * cellSize

    // Create the square with walls based on the cell's wall state
    square = Square(x, y, cellSize, SquareWalls(cell), Some(cell.coords))
  } yield square

  stage = new JFXApp.PrimaryStage {
    title = "Square Maze"
    scene = new Scene(cols * cellSize, rows * cellSize) {
      // Render both hexagons and their walls, and add optional coordinates as text
      val shapes = squares.flatMap { square =>
        Seq(square.toRectangle) ++ square.renderWalls ++ square.renderCoordinates.toSeq
      }
      content = shapes
    }
  
  }
}