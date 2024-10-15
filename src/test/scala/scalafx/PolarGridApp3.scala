import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color

object PolarGridApp3 extends JFXApp {

  val canvasWidth = 400
  val canvasHeight = 400
  val centerX = canvasWidth / 2
  val centerY = canvasHeight / 2
  val cellCount = 60
  // val numRings = 10
  // val numLines = 12
  val numLines = Math.ceil(Math.sqrt(cellCount)).toInt
  val numRings = Math.ceil(cellCount.toDouble / numLines).toInt

  // Function to draw polar grid
  def drawPolarGrid(gc: GraphicsContext): Unit = {
    gc.setStroke(Color.Gray)
    gc.setLineWidth(1)

    // Draw concentric circles
    for (i <- 1 to numRings) {
      val radius = i * (canvasWidth / (2 * numRings))
      gc.strokeOval(centerX - radius, centerY - radius, radius * 2, radius * 2)
    }

    // Draw radial lines
    for (i <- 0 until numLines) {
      val angle = 2 * Math.PI * i / numLines // theta
      val x = centerX + Math.cos(angle) * (canvasWidth / 2)
      val y = centerY + Math.sin(angle) * (canvasHeight / 2)
      gc.strokeLine(centerX, centerY, x, y)
    }
  }

  // Set up the stage
  stage = new JFXApp.PrimaryStage {
    title = "Polar Grid"
    scene = new Scene {
      val canvas = new Canvas(canvasWidth, canvasHeight)
      val gc: GraphicsContext = canvas.graphicsContext2D

      // Draw the polar grid
      drawPolarGrid(gc)

      // Set up the layout
      content = new StackPane {
        children = List(canvas)
      }
    }
  }
}
