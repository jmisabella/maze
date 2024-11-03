package square

import maze.behaviors.builders.Generator
import maze.classes.{ Cell, Coordinates, MazeRequest }
import maze.classes.Algorithm._
import maze.classes.MazeType._
import maze.classes.direction.SquareDirection._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.{ Rectangle, Line }
import play.api.libs.json.{ Json, Format }
import maze.behaviors.builders.RecursiveBacktracker

case class Square(x: Double, y: Double, size: Double, walls: SquareWalls) {
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
  val request = MazeRequest(Orthogonal, cols, rows, RecursiveBacktracker, Coordinates(0, 0), Coordinates(0, 0))
  val maze = Generator.generate(request).cells

  stage = new JFXApp.PrimaryStage {
    title = "Square Maze"
    scene = new Scene(cols * cellSize, rows * cellSize) {
      val shapes = for {
        row <- maze.indices
        col <- maze(row).indices
        cell = maze(row)(col)

        x = col * cellSize
        y = row * cellSize

        // Create the square with walls based on the cell's wall state
        square = Square(x, y, cellSize, SquareWalls(cell))

        rectangle = square.toRectangle
        walls = square.renderWalls // Generate the wall lines
      } yield Seq(rectangle) ++ walls

      content = shapes.flatten
    }
  }
}