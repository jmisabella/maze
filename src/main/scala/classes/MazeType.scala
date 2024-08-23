// package maze.classes

// import play.api.libs.json.{ Json, Format }

// object MazeType extends Enumeration {
//   type MazeType = Value
//   val Unsolved,
//     DistanceMap,
//     Solved = Value
  
//   implicit val format: Format[MazeType] = Json.formatEnum(this)

//   def fromString(s: String): Option[MazeType] = values.find(_.toString.toLowerCase == s.replace("-", "").replace("_", "").toLowerCase())

// }

