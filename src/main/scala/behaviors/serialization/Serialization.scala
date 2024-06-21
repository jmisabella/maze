package maze.behaviors.serialization

trait Serialization[A] {
  def parse(json: String): Either[String, A]
  def json(items: A): String
}