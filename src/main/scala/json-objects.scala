sealed trait JsonObject[T] {
  def get: T
	def getT[T1]: T1 = get.asInstanceOf[T1]
}

sealed abstract class JsonBasicObject[T](value: T) extends JsonObject[T] {
  def get = value
}

/* number */
class JsonNumber(value: Double) extends JsonBasicObject[Double](value) {
  override def toString = "JsonNumber [" + value + "]"
}

/* boolean */
sealed class JsonBoolean(value: Boolean) extends JsonBasicObject[Boolean](value) {
  override def toString = "JsonBoolean [" + value + "]"
}

object JsonTrue extends JsonBoolean(true)
object JsonFalse extends JsonBoolean(false)

/* string */
class JsonString(value: String) extends JsonBasicObject[String](value) {
  override def toString = "JsonString [" + value + "]"
}

/* list */
class JsonList(value: List[JsonObject[_]]) extends JsonObject[List[Any]] {
  def ::(item: JsonObject[_]): JsonList = new JsonList(item :: value)
	def get: List[Any] = value.map(_.get)
	def isEmpty = value.isEmpty
	override def toString = "JsonList [" + value + "]"
}
object JsonEmptyList extends JsonList(List.empty[JsonObject[_]])

/* map */
class JsonMap(value: Map[JsonString, JsonObject[_]]) extends JsonObject[Map[String, Any]] {
  def +(tuple: (JsonString, JsonObject[_])): JsonMap = new JsonMap(value + tuple)
	def get: Map[String, Any] = value.map(t => (t._1.get, t._2.get))
	def isEmpty = value.isEmpty
	override def toString = "JsonMap [" + value + "]"
}
object JsonEmptyMap extends JsonMap(Map.empty[JsonString, JsonObject[_]])
