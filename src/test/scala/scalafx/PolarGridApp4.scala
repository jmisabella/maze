import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Path, MoveTo, LineTo, CubicCurveTo}
import scalafx.scene.Group

object PolarGridApp4 extends JFXApp {

  val canvasWidth = 400
  val canvasHeight = 400
  val centerX = canvasWidth / 2
  val centerY = canvasHeight / 2
  // val cellCount = 60
  val cellCount = 20
  val numLines = Math.ceil(Math.sqrt(cellCount)).toInt
  val numRings = Math.ceil(cellCount.toDouble / numLines).toInt

  // Function to draw polar grid
  def drawPolarGrid(): Group = {
    val group = new Group()

    // Draw each cell using Path
    for (i <- 1 to numRings) {
      val innerRadius = (i - 1) * (canvasWidth / (2 * numRings))
      val outerRadius = i * (canvasWidth / (2 * numRings))

      for (j <- 0 until numLines) {
        val angle1 = 2 * Math.PI * j / numLines
        val angle2 = 2 * Math.PI * (j + 1) / numLines

        val innerX1 = centerX + (Math.cos(angle1) * innerRadius)
        val innerY1 = centerY + (Math.sin(angle1) * innerRadius)
        val innerX2 = centerX + (Math.cos(angle2) * innerRadius)
        val innerY2 = centerY + (Math.sin(angle2) * innerRadius)

        val outerX1 = centerX + (Math.cos(angle1) * outerRadius)
        val outerY1 = centerY + (Math.sin(angle1) * outerRadius)
        val outerX2 = centerX + (Math.cos(angle2) * outerRadius)
        val outerY2 = centerY + (Math.sin(angle2) * outerRadius)

        // Create the path for the cell
        val path = new Path()
        path.getElements.add(MoveTo(innerX1, innerY1))
        path.getElements.add(LineTo(outerX1, outerY1))
        path.getElements.add(CubicCurveTo(
          (innerX1 + outerX1) / 2, (innerY1 + outerY1) / 2,
          (innerX2 + outerX2) / 2, (innerY2 + outerY2) / 2,
          innerX2, innerY2
        ))
        path.getElements.add(LineTo(innerX2, innerY2))
        path.getElements.add(LineTo(innerX1, innerY1))

        // Set stroke properties
        path.setStroke(Color.Gray)
        path.setStrokeWidth(1)

        // Add the path to the group
        group.children.add(path)
      }
    }

    group
  }

  // Set up the stage
  stage = new JFXApp.PrimaryStage {
    title = "Polar Grid"
    scene = new Scene {
      val canvas = new Canvas(canvasWidth, canvasHeight)
      val gc: GraphicsContext = canvas.graphicsContext2D

      // Draw the polar grid
      val polarGrid = drawPolarGrid()

      // Set up the layout
      content = new StackPane {
        children = List(canvas, polarGrid)
      }
    }
  }
}