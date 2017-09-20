package chapter9


import scala.util.matching.Regex

case class **[A,B](a:A, b:B)
case class Location(input: String, offset: Int = 0) {

  lazy val line: Int =
    input.slice(0,offset+1).count(_ == '\n') + 1

  lazy val col: Int =
    input.slice(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

  def toError(msg:String):ParseError =
    ParseError(List((this,msg)))

  def advanceBy(n:Int):Location =
    Location(input, offset + n)

  def remaining:Int =
    input.length - offset
}

case class ParseError(stack: List[(Location, String)]) {
  def push(other:ParseError):ParseError =
    copy( stack = stack ::: List(other.stack.head) )
}


abstract class Parsers[AParser[+_]] { self =>

  def errorLocation(e: ParseError): Location =
    e.stack.head._1

  def errorMessage(e: ParseError): String =
    e.stack.head._2

  def run[A](p:AParser[A])(s:String):Either[ParseError, A]

  implicit def operators[A](p:AParser[A]):ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a:A)( implicit f: A => AParser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def string(s:String):AParser[String]
  implicit def regex(r:Regex):AParser[String]


  def attempt[A](p: => AParser[A]):AParser[A]
  def or[A](p1: => AParser[A], p2: => AParser[A]):AParser[A]

  def succeed[A](a:A):AParser[A]
  def fail[A](msg:String):AParser[A]

  def label[A](msg:String)(p:AParser[A]):AParser[A]
  def scope[A](msg:String)(p:AParser[A]):AParser[A]

  def flatMap[A,B](pa: => AParser[A])(f: A => AParser[B]):AParser[B]
  def slice[A](p:AParser[A]):AParser[String]

  def map[A,B](pa: => AParser[A])(f:A => B):AParser[B] =
    pa flatMap (f andThen succeed)

  def product[A,B](pa:AParser[A],pb: => AParser[B]):AParser[A ** B] =
    pa flatMap ( a => pb.map (**(a,_)))

  def dropLeft[A,B](left:AParser[A], right: => AParser[B]):AParser[B] =
    map2(left, right)((_,b) => b)

  def dropRight[A,B](left:AParser[A], right: => AParser[B]):AParser[A] =
    map2(left, right)((a, _) => a)

  def map2[A,B,C](pa: => AParser[A], pb: => AParser[B])(f:(A,B) => C):AParser[C] =
    pa flatMap (a => pb map (f(a, _)))

  def many[A](p: => AParser[A]):AParser[List[A]] =
    attempt(p flatMap( a => many(p) map (a :: _))).or(succeed(List.empty[A]))

  def many1[A](p: => AParser[A]):AParser[List[A]] =
    p flatMap( a => many(p) map (a :: _))

  def sep1[A,B](p: => AParser[A], separator:AParser[B]):AParser[List[A]] =
    p ** many1(separator *> p) map { case a ** as => a :: as }

  def sep[A,B](p: => AParser[A], separator: AParser[B]):AParser[List[A]] =
    attempt(sep1(p, separator)) | succeed (List.empty[A])

  def listOfN[A](n:Int, p: => AParser[A]):AParser[List[A]] =
    if(n <= 0) succeed(List.empty[A])
    else attempt(map2(p, listOfN(n - 1, p))(_ :: _))

  def char(c:Char):AParser[Char] =
    string(c.toString) map (_.head)

  def anyChar:AParser[String] =
    regex(""".""".r)

  def satisfies(p:String => Boolean, errorMsg:String => String):AParser[String] =
    anyChar flatMap (c => if(p(c)) succeed(c) else fail(errorMsg(c)))

  def anyOf(cs:String):AParser[String] =
    satisfies(cs.contains, c => s"""$c not in 'anyOf(${cs.mkString(",")})""")

  def noneOf(cs:String):AParser[String] =
    satisfies(!cs.contains(_), c => s"""$c not allowed by 'noneOf(${cs.mkString(",")})""")

  def whitespace:AParser[String] =
    """\s""".r

  def newline:AParser[String] =
    "\n"

  def separators:AParser[List[String]] =
    many(whitespace | newline)

  def opt[A](p:AParser[A]):AParser[Option[A]] =
    attempt(p.map(Some(_))) or succeed(None)

  case class ParserOps[A](p:AParser[A]) {

    def |[B >: A](p2: => AParser[B]):AParser[B] = self.or(p, p2)
    def or[B >: A](p2: =>AParser[B]):AParser[B] = self.or(p, p2)

    def **[B](p2: => AParser[B]):AParser[A ** B] = self.product(p, p2)
    def product[B](p2: => AParser[B]):AParser[A ** B] = self.product(p, p2)

    def *>[B](p2: => AParser[B]):AParser[B] = self.dropLeft(p,p2)
    def <*[B](p2: => AParser[B]):AParser[A] = self.dropRight(p, p2)

    def map[B](f:A => B):AParser[B] = self.map(p)(f)
    def flatMap[B](f: A => AParser[B]):AParser[B] = self.flatMap(p)(f)

    def many:AParser[List[A]] = self.many(p)

  }

  object Laws {

    def orAssociative[A](p1:AParser[A], p2:AParser[A], p3:AParser[A], s:String):Boolean =
      run(p1 | (p2 | p3))(s) == run((p1 | p2) | p3)(s)

    def orCommutative[A](p1:AParser[A], p2:AParser[A], s:String):Boolean =
      run(p1 | p2)(s) == run(p2 | p1)(s)

    def productAssociative[A,B,C](pa:AParser[A], pb:AParser[B], pc:AParser[C], s:String):Boolean =
      run(pa ** (pb ** pc))(s) == run((pa ** pb) ** pc)(s)

    def succeedIdentity[A](a:A, s:String):Boolean =
      run(succeed(a))(s) == Right(a)

  }

}

sealed abstract class Result[+A] {
  def fold[B](success: Success[A] => B, failed: Failure => B):B = this match {
    case s@Success(_,_) => success(s)
    case f@Failure(_,_) => failed(f)
  }

  def mapError(f:ParseError => ParseError):Result[A] = this match {
    case  s@Success(_,_) => s
    case    Failure(e,c) => Failure(f(e), c)
  }

  def uncommit:Result[A] = this match {
    case s@Success(_,_) => s
    case  Failure(e,_)  => Failure(e)
  }

  def addCommit(committed:Boolean):Result[A] = this match {
    case Success(a, n) => Success(a, n)
    case Failure(e, p) => Failure(e, p || committed)
  }

  def advanceSuccess(by:Int):Result[A] = this match {
    case Success(a, n) => Success(a, n + by)
    case f => f
  }

}

case class Success[+A](get:A, charsConsumed:Int) extends Result[A]
case class Failure(get:ParseError, committed:Boolean = false) extends Result[Nothing]
case class Parser[+A](run:Location => Result[A])

object Parser extends Parsers[Parser] {
  override def run[A](p: Parser[A])(s: String): Either[ParseError, A] =
    p.run(Location(s)).fold(s => Right(s.get), f => Left(f.get))

  override implicit def string(s: String): Parser[String] =
    Parser { loc =>
      if(loc.input.substring(loc.offset).startsWith(s))
        Success(s, s.length)
      else
        Failure(loc.toError(s"expected '$s'"))
    }

  override implicit def regex(r: Regex): Parser[String] =
    Parser { loc =>
      r.findPrefixMatchOf(loc.input.substring(loc.offset))
        .fold[Result[String]](Failure(loc.toError(s"'$r' did not match input")))(
        m => Success(m.group(0), m.group(0).length))
    }

  override def attempt[A](p: => Parser[A]): Parser[A] =
    Parser( loc => p.run(loc).uncommit )

  override def or[A](p1: => Parser[A], p2: => Parser[A]): Parser[A] =
    Parser { loc => p1.run(loc) match {
        case Success(a, consumed)        => Success(a, consumed)
        case Failure(parserError, true)  => Failure(parserError, committed = true)
        case           Failure(_, false) => p2.run(loc)
      }
    }

  override def succeed[A](a: A): Parser[A] =
    Parser { _ => Success(a, 0) }

  override def fail[A](msg: String): Parser[A] =
    Parser { loc => Failure(loc.toError(msg)) }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    Parser { loc => p.run(loc).mapError(_ => loc.toError(msg)) }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    Parser { loc => p.run(loc).mapError( e => loc.toError(msg).push(e)) }

  override def flatMap[A, B](pa: => Parser[A])(f: A => Parser[B]): Parser[B] =
    Parser { loc =>
      pa.run(loc) match {
        case Success(a, n) => f(a).run(loc.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case e@Failure(_,_) => e
      }
    }

  override def slice[A](p: Parser[A]): Parser[String] =
    Parser { loc =>
      p.run(loc) match {
        case Success(_, consumed) => Success(loc.input.substring(loc.offset, consumed), consumed)
        case e@Failure(_,_) => e
      }
    }
}