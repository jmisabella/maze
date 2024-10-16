package hex 

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Polygon
import java.util.Arrays

// Hexagon class to define the hex cell properties
case class Hexagon(x: Double, y: Double, size: Double) {
  def points: Array[Double] = {
    Array.tabulate(6) { i =>
      val angle = 2 * Math.PI / 6 * i // No offset needed for flat-topped hexagons
      Array(x + size * Math.cos(angle), y + size * Math.sin(angle))
    }.flatten
  }

  def toPolygon: Polygon = {
    val polygon = new Polygon()
    // Convert Scala Double to Java Double
    polygon.getPoints.addAll(Arrays.asList(points.map(java.lang.Double.valueOf): _*))
    polygon.fill = Color.Transparent
    polygon.stroke = Color.Black
    polygon
  }
}

// Cell class for storing cell position in the grid
case class Cell(row: Int, col: Int)

object HexGridApp extends JFXApp {
  val rows = 10
  val cols = 10
  val cellSize = 30 // Size of each hex cell
  val hexHeight = Math.sqrt(3) * cellSize / 2 // Adjusted height for flat-topped hexagons

  stage = new JFXApp.PrimaryStage {
    title = "Flat-Topped Hex Grid"
    scene = new Scene(cols * cellSize * 1.5, rows * hexHeight * 2 + cellSize) {
      val shapes = for {
        row <- 0 until rows
        col <- 0 until cols
        // Calculate position for flat-topped hexagons
        x = col * cellSize * 1.5 // Horizontal spacing for flat-topped hexagons
        y = row * hexHeight * 2 + (if (col % 2 == 0) 0 else hexHeight) // Stagger odd columns vertically

        hexagon = Hexagon(x, y, cellSize)
        polygon = hexagon.toPolygon
      } yield polygon

      content = shapes
    }
  }
}
