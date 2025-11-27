package ch.usi.si.msde.edsl.assignment_03.model

object JsonModel:

  sealed trait JsonValue

  case object JsonNull extends JsonValue:
    override def toString() = "null"

  case class JsonObject(
      val values: List[(String, JsonValue)] = List()
  ) extends JsonValue:
    override def toString() =
      val valueRep = values.map { case (k, v) => s"\"${k}\": ${v}" }
      s"""{ ${valueRep.mkString(", ")} }"""
    lazy val valueMap: Map[String, JsonValue] = Map(values*)
  end JsonObject

  case class JsonArray(val values: List[JsonValue]) extends JsonValue:
    override def toString() =
      values.map { case v => s"${v}" }.mkString("[", ", ", "]")
  end JsonArray

  case class JsonString(val value: String) extends JsonValue:
    override def toString() = s"\"$value\""

  case class JsonNumber(val value: Double) extends JsonValue:
    override def toString() = s"$value"

  sealed trait JsonBoolean extends JsonValue:
    val value: Boolean

  case object JsonTrue extends JsonBoolean:
    override def toString() = "true"
    override val value = true

  case object JsonFalse extends JsonBoolean:
    override def toString() = "false"
    override val value = false

end JsonModel
