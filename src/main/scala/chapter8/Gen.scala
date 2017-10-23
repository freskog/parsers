package chapter8

import chapter6.{RNG, RandFunctions}
import chapter5.Stream

abstract class GenFunctions[F[+_]] extends RandFunctions[F] {

  case class Interval(start:Double, end:Double) {
    def contains(x:Double):Boolean = x >= start && x < end
  }

  def optionOf[A](fa:F[A]):F[Option[A]] =
    weighted((unit(None),20.0),(fa map (Option(_)),80))

  def listOf[A](fa:F[A]):F[List[A]] =
    nonNegativeLessThan(100) flatMap (a => listOfN(a, fa))

  def listOfN[A](n:Int, fa:F[A]):F[List[A]] =
    if(n == 0) unit(List.empty[A])
    else map2(fa, listOfN(n - 1, fa))(_ :: _)

  def choose[A](start:Int, stopExclusive:Int): F[Int] =
    int map (_ % (stopExclusive - start)) map ( _ + start)

  def alpha:F[Char] =
    choose('a'.toInt,'z'.toInt + 1) map (_.toChar)

  def alphaNum:F[Char] =
    union(alpha,choose('0'.toInt,'9'.toInt+1) map (_.toChar))

  def string:F[String] =
    listOf(alphaNum).map(_.mkString)

  def boolean:F[Boolean] =
    int map (_ % 2 == 0)

  def union[A](f1:F[A],f2:F[A]):F[A] =
    boolean flatMap ( usef1 => if(usef1) f1 else f2)

  def weighted[A](f1:(F[A],Double),f2:(F[A],Double)):F[A] =
    (f1,f2) match {
      case ((fst,w1), (snd,w2)) =>
        val i1 = if((w1 / (w1+w2)) > (w2 / (w1+w2))) Interval(0, w1 / (w1 + w2)) else Interval(1-(w1/(w1+w2)),1)
        double flatMap( p => if(i1.contains(p)) fst else snd)
    }

  def forAll[A](fa:F[A])(p: A => Boolean):Prop

}


case class Prop(check: (Int,RNG) => Result) {
  def ||(other:Prop):Prop =
    Prop { (n, rng) =>
      (check(n, rng), other.check(n, rng)) match {
        case (Passed, _) | (_, Passed) => Passed
        case (Falsified(msg1, successes1), Falsified(msg2, successes2)) => Falsified(s"$msg1, $msg2", successes1 + successes2)
      }
    }

  def &&(other:Prop):Prop =
    Prop { (n, rng) =>
      (check(n, rng), other.check(n, rng)) match {
        case (f1@Falsified(_, _),f2@Falsified(_, _)) => Falsified(s"${f1.failure} and ${f2.failure}", f1.successes + f2.successes)
        case (f@Falsified(_, _), _) => f
        case (_, f@Falsified(_, _)) => f
        case (Passed, Passed) => Passed
      }
    }
}

sealed abstract class Result extends Product with Serializable {
  def isFalsified:Boolean = this match {
    case Passed => false
    case Falsified(_,_) => true
  }
}
case object Passed extends Result
case class Falsified(failure: String, successes:Int) extends Result

case class Gen[+A](run:RNG => (RNG, A))
object Gen extends GenFunctions[Gen] {

  def apply[A](implicit G:Gen[A]):Gen[A] = G

  override def forAll[A](fa: Gen[A])(p: A => Boolean):Prop = Prop { (n,rng) =>
    randomStream(fa)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (p(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g:Gen[A])(rng:RNG):Stream[A] =
   Stream.unfold(rng)(rng => Option(g.run(rng)))

  def buildMsg[A](a: A, e:Exception): String =
    s"""
       |test case: $a
       |generated an exception: ${e.getMessage}
       |stack track:
       |  ${e.getStackTrace.mkString("\t\n")}
     """.stripMargin

  override def instance[A](f: RNG => (RNG, A)) =
    Gen(f)

  override def run[A](fa: Gen[A])(s: RNG) =
    fa.run(s)
}

case class SGen[+A](forSize: Int => Gen[A])
