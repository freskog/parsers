package chapter9


trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[AParser[+_]](P:Parsers[AParser]): AParser[JSON] = {
    import P._

    def sign:AParser[String] =
      opt("-") map (_.getOrElse(""))

    def digit1to9:AParser[String] =
      "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

    def digit:AParser[String] =
      "0" | digit1to9

    def digits: AParser[List[String]] =
      many(digit)

    def int:AParser[String] =
      (sign ** digit1to9 ** digits).map { case sign ** d1to9 ** ds => ds.mkString(s"$sign$d1to9", "", "") } |
      (sign ** digit).map { case sign ** d => s"$sign$d" }

    def frac:AParser[String] =
      "." ** digits map { case per ** ds => ds.mkString(per,"","")}

    def exp:AParser[String] =
      ("e" | "E") ** ("+" | "-" | "") ** many1(digit) map { case e ** sign ** ds => ds.mkString(s"$e$sign","","") }

    def hex:AParser[String] =
      """[0-9a-fA-F]""".r

    def unicodeChar:AParser[String] =
      "u" *> listOfN(4,hex) map (hexes => Integer.parseInt(hexes.mkString(""), 16).toChar.toString)

    def backslash:AParser[String] =
      string("\\")

    def slash:AParser[String] =
      string("/")

    def bell:AParser[String] =
      string("b") map (_ => "\b")

    def formFeed:AParser[String] =
      string("f") map (_ => "\f")

    def newLine:AParser[String] =
      string("n") map (_ => "\n")

    def carriageReturn:AParser[String] =
      string("r") map ( _ => "\r")

    def tab:AParser[String] =
      string("t") map ( _ => "\t")

    def controlChar:AParser[String] =
      "\\" ** (unicodeChar | backslash | slash | bell | formFeed | newLine | carriageReturn | tab) map { case _ ** res => res }

    def jString:AParser[JString] =
      "\"" *> many(attempt(controlChar) or noneOf("\"")).map(_.mkString).map(JString) <* "\""

    def jBool:AParser[JBool] =
      ("true" | "false").map(_.toBoolean).map(JBool)

    def jNull:AParser[JNull.type] =
      "null" *> succeed(JNull)

    def jNumber:AParser[JNumber] =
      (
            attempt(int ** frac ** exp).map { case i ** f ** e => s"$i$f$e".toDouble }
          | attempt(int ** frac       ).map { case i ** f      => s"$i$f".toDouble }
          | attempt(int ** exp        ).map { case i ** e      => s"$i$e".toDouble }
          | int.map(_.toDouble)
        ).map(JNumber)

    def jPair:AParser[(String, JSON)] =
      ((jString <* separators ** ":" ** separators) ** jValue) map { case JString(key) ** value => key -> value }

    def members:AParser[List[(String, JSON)]] =
      attempt(sep1(jPair, separators ** "," ** separators)) or attempt(jPair.map(List(_))) or succeed(Nil)

    def values:AParser[List[JSON]] =
      attempt(sep1(jValue, separators ** "," ** separators)) or attempt(jValue.map(List(_))) or succeed(Nil)

    def jObject:AParser[JObject] =
      attempt("{" ** separators *> members <* separators ** "}") map (members => JObject(members.toMap[String, JSON]))

    def jArray:AParser[JArray] =
      attempt("[" ** separators *> values <* "]") map (elements => JArray(elements.toArray[JSON]))

    def jValue:AParser[JSON] =
      attempt(jString) | attempt(jNumber) | attempt(jBool) | attempt(jNull) | attempt(jObject) | attempt(jArray)

    separators *> (jObject | jArray) <* separators
  }
}