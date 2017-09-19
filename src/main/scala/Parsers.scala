import javax.swing.JPopupMenu.Separator

import scala.util.matching.Regex

case class **[A,B](a:A, b:B)

abstract class Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p:Parser[A])(s:String):Either[ParseError, A]

  implicit def operators[A](p:Parser[A]):ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a:A)( implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def string(s:String):Parser[String]
  implicit def regex(r:Regex):Parser[String]

  def or[A](p1:Parser[A], p2: => Parser[A]):Parser[A]
  def succeed[A](a:A):Parser[A]
  def flatMap[A,B](pa:Parser[A])(f: A => Parser[B]):Parser[B]
  def slice[A](p:Parser[A]):Parser[String]

  def map[A,B](pa:Parser[A])(f:A => B):Parser[B] =
    flatMap(pa)( f andThen succeed )

  def product[A,B](pa:Parser[A],pb: => Parser[B]):Parser[A ** B] =
    flatMap(pa)( a => map(pb)( b => **(a,b)))

  def dropLeft[A,B](left:Parser[A], right: => Parser[B]):Parser[B] =
    map2(left, right)((_,b) => b)

  def dropRight[A,B](left:Parser[A], right: => Parser[B]):Parser[A] =
    map2(left, right)((a, _) => a)

  def map2[A,B,C](pa:Parser[A], pb: => Parser[B])(f:(A,B) => C):Parser[C] =
    flatMap(pa)( a => map(pb)(b => f(a,b)))

  def many[A](p: => Parser[A]):Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty[A])

  def many1[A](p: => Parser[A]):Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def sep1[A,B](p: => Parser[A], separator:Parser[B]):Parser[List[A]] =
    p ** many1(separator *> p) map { case a ** as => a :: as }

  def sep[A,B](p: => Parser[A], separator: Parser[B]):Parser[List[A]] =
    sep1(p, separator) | succeed (List.empty[A])

  def listOfN[A](n:Int, p: => Parser[A]):Parser[List[A]] =
    if(n <= 0) succeed(List.empty[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def char(c:Char):Parser[Char] =
    string(c.toString) map (_.head)

  def anyChar:Parser[String] =
    regex(""".""".r)

  def whitespace:Parser[String] =
    """\s""".r

  def whitespaces:Parser[List[String]] =
    many(whitespace)

  case class ParserOps[A](p:Parser[A]) {

    def |[B >: A](p2: => Parser[B]):Parser[B] = self.or(p, p2)
    def or[B >: A](p2: =>Parser[B]):Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]):Parser[A ** B] = self.product(p, p2)
    def product[B](p2: => Parser[B]):Parser[A ** B] = self.product(p, p2)

    def *>[B](p2: => Parser[B]):Parser[B] = self.dropLeft(p,p2)
    def <*[B](p2: => Parser[B]):Parser[A] = self.dropRight(p, p2)

    def map[B](f:A => B):Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]):Parser[B] = self.flatMap(p)(f)

    def many:Parser[List[A]] = self.many(p)

  }

  object Laws {

    def orAssociative[A](p1:Parser[A], p2:Parser[A], p3:Parser[A], s:String):Boolean =
      run(p1 | (p2 | p3))(s) == run((p1 | p2) | p3)(s)

    def orCommutative[A](p1:Parser[A], p2:Parser[A], s:String):Boolean =
      run(p1 | p2)(s) == run(p2 | p1)(s)

    def productAssociative[A,B,C](pa:Parser[A], pb:Parser[B], pc:Parser[C], s:String):Boolean =
      run(pa ** (pb ** pc))(s) == run((pa ** pb) ** pc)(s)

    def succeedIdentity[A](a:A, s:String):Boolean =
      run(succeed(a))(s) == Right(a)

  }

}
