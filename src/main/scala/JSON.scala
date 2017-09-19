
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P:Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    def sign:Parser[String] =
      "-" | ""

    def digit1to9:Parser[String] =
      "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

    def digit:Parser[String] =
      "0" | digit1to9

    def digits: Parser[List[String]] =
      many(digit)

    def int:Parser[String] =
      (sign ** digit1to9 ** digits).map { case sign ** d1to9 ** ds => ds.mkString(s"$sign$d1to9", "", "") } |
      (sign ** digit).map { case sign ** d => s"$sign$d" }

    def frac:Parser[String] =
      "." ** digits map { case per ** ds => ds.mkString(per,"","")}

    def exp:Parser[String] =
      ("e" | "E") ** ("+" | "-" | "") ** many1(digit) map { case e ** sign ** ds => ds.mkString(s"$e$sign","","") }

    def hex:Parser[String] =
      """[0-9a-fA-F]""".r

    def unicodeChar:Parser[String] =
      "u" ** listOfN(4,hex) map { case "u" ** hexes => Integer.parseInt(hexes.mkString(""),16).toChar.toString }

    def backslash:Parser[String] =
      string("\\")

    def slash:Parser[String] =
      string("/")

    def bell:Parser[String] =
      string("b") map (_ => "\b")

    def formFeed:Parser[String] =
      string("f") map (_ => "\f")

    def newLine:Parser[String] =
      string("n") map (_ => "\n")

    def carriageReturn:Parser[String] =
      string("r") map ( _ => "\r")

    def tab:Parser[String] =
      string("t") map ( _ => "\t")

    def controlChar:Parser[String] =
      "\\" ** (unicodeChar | backslash | slash | bell | formFeed | newLine | carriageReturn | tab) map { case _ ** res => res }

    def jString:Parser[JString] =
      "\"" *> many(controlChar | anyChar).map(_.mkString).map(JString) <* "\""

    def jBool:Parser[JBool] =
      ("true" | "false").map(_.toBoolean).map(JBool)

    def jNull:Parser[JNull.type ] =
      "null" *> succeed(JNull)

    def jNumber:Parser[JNumber] =
      (
            (int ** frac ** exp).map { case i ** f ** e => s"$i$f$e".toDouble }
          | (int ** frac       ).map { case i ** f      => s"$i$f".toDouble }
          | (int ** exp        ).map { case i ** e      => s"$i$e".toDouble }
          | int.map(_.toDouble)
        ).map(JNumber)

    def jPair:Parser[(String, JSON)] =
      ((jString <* whitespaces ** ":" ** whitespaces) ** jValue) map { case JString(key) ** value => key -> value }

    def members:Parser[List[(String, JSON)]] =
      sep(jPair, whitespaces ** "," ** whitespaces)

    def jObject:Parser[JObject] =
      ("{" ** whitespaces *> members <* whitespaces ** "}") map (members => JObject(members.toMap[String, JSON]))

    def jArray:Parser[JArray] =
      ("[" ** whitespaces *> sep(jValue, whitespaces ** "," ** whitespaces) <* "]") map (elements => JArray(elements.toArray))

    def jValue:Parser[JSON] =
      jString | jNumber | jBool | jNull | jObject | jArray

    whitespaces *> (jObject | jArray) <* whitespaces
  }
}