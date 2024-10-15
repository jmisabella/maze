import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Arc
import scalafx.scene.layout.Pane

object SemiArcApp extends JFXApp {
  
//   val semiArcPane = createThickSemiArc(300, 300, 100, 150, 0, 180, counterClockwise = false)

  // Set up the stage
  stage = new JFXApp.PrimaryStage {
    title = "Polar Grid"
    scene = new Scene(1200, 1200) {
    //   val arc1 = Arc(250, 150, 40, 30, 0, 270)
      val arc1 = Arc(100, 100, 180, 180, 45, 300)
      val arc2 = Arc(700, 700, 180, 180, 0, 90)
    //   content = List(arc1, arc2)
      content = List(arc2)
    }
  }

}