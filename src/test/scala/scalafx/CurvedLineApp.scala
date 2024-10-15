import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.{CubicCurveTo, LineTo, MoveTo, Path}
import scalafx.stage.Stage

object BezierCurveExample extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title = "Bézier Curve Example"
    scene = new Scene(400, 400) {
      val pane = new Pane()
      content = pane

      // Create a Path for the Bézier curve
      val bezierPath = new Path {
        // Move to the starting point
        elements.add(MoveTo(50, 200)) 
        // Define the cubic Bézier curve
        // elements.add(CubicCurveTo(150, 50, 250, 50, 350, 200))
        elements.add(CubicCurveTo(100, 100, 250, 50, 350, 200))
        
        stroke = Color.BLUE
        strokeWidth = 4
        fill = Color.Transparent // Do not fill the path
      }

      // Add the path to the pane
      pane.children.add(bezierPath)
    }
  }
}