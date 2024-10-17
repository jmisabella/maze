package hex2

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Polygon
import java.util.Arrays

case class Cell(row: Int, col: Int)

// Hexagon representation
case class Hexagon(centerX: Double, centerY: Double, size: Double) {
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
    polygon.stroke = Color.Black
    polygon
  }
}

object HexGridApp2 extends JFXApp {
  val cellSize = 35.0 // Size of the hexagon (distance from center to any corner)
  val hexHeight = Math.sqrt(3) * cellSize // Height of a hexagon
  val hexWidth = 2 * cellSize // Width of a hexagon
  val rows = 10 // Logical grid rows
  val cols = 10 // Logical grid columns

  // Convert logical row, col to pixel-based center of the hexagon
  def hexCenter(row: Int, col: Int): (Double, Double) = {
    val x = col * 3.0 / 2.0 * cellSize  // Horizontal distance between hex centers
    val y = row * hexHeight + (if (col % 2 == 1) hexHeight / 2 else 0) // Staggered rows for odd cols
    (x, y)
  }

  // Create hexagon cells based on integer row, col coordinates
  val hexagons = for {
    row <- 0 until rows
    col <- 0 until cols
    (x, y) = hexCenter(row, col)
    hexagon = Hexagon(x + cellSize, y + cellSize, cellSize) // Adjust by size for canvas centering
  } yield hexagon

  stage = new JFXApp.PrimaryStage {
    title = "Hex Grid with Integer Coordinates"
    scene = new Scene(cols * hexWidth, rows * hexHeight) {
      val shapes = hexagons.map(_.toPolygon)
      content = shapes
    }
  }
}

