import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line
import scalafx.scene.Group

object PolarGridMaze extends JFXApp {

  // Function to draw a polar grid
  def drawPolarGrid(centerX: Double, centerY: Double, radius: Double, numRings: Int, numSpokes: Int): Group = {
    val group = new Group()

    // Draw rings
    for (i <- 1 to numRings) {
      val r = radius * i / numRings
      val ring = Line(centerX - r, centerY, centerX + r, centerY)
      group.children.add(ring)
      group.children.add(Line(centerX, centerY - r, centerX, centerY + r))
    }

    // Draw spokes
    for (i <- 0 until numSpokes) {
      val angle = 2 * Math.PI * i / numSpokes
      val endX = centerX + radius * Math.cos(angle)
      val endY = centerY + radius * Math.sin(angle)
      group.children.add(Line(centerX, centerY, endX, endY))
    }

    group
  }

  // Function to create a simple maze (for demonstration purposes)
  def drawMaze(centerX: Double, centerY: Double, radius: Double): Group = {
    val mazeGroup = new Group()

    // Simple example of maze walls (randomly placed)
    val walls = Seq(
      (0.5, 1.0), (1.0, 0.5), (1.5, 1.0), (1.0, 1.5), (1.5, 1.5)
    )

    walls.foreach { case (angle, distance) =>
      val startX = centerX + distance * Math.cos(angle * Math.PI / 2)
      val startY = centerY + distance * Math.sin(angle * Math.PI / 2)
      val endX = centerX + distance * Math.cos(angle * Math.PI / 2 + Math.PI / 4)
      val endY = centerY + distance * Math.sin(angle * Math.PI / 2 + Math.PI / 4)
    //   mazeGroup.children.add(Line(startX, startY, endX, endY) {
    //     stroke = Color.Black
    //     strokeWidth = 2
    //   })
      mazeGroup.children.add(Line(startX, startY, endX, endY))
    }

    mazeGroup
  }

  stage = new JFXApp.PrimaryStage {
    title = "Polar Grid Maze"
    scene = new Scene(800, 800) {
      val centerX = width / 2
      val centerY = height / 2
      val radius = Math.min(width.toDouble, height.toDouble) / 2 * 0.8

      val grid = drawPolarGrid(centerX.toDouble, centerY.toDouble, radius, 8, 16)
      val maze = drawMaze(centerX.toDouble, centerY.toDouble, radius)

      content = new Group(grid, maze)
    //   content = new Group(grid)
    //   content = List(grid, maze)
    //   content = List(maze)
    }
  }
}