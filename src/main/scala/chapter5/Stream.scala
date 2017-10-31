package chapter5

sealed abstract class Stream[+A] {

  import Stream.cons

  def map[B](f:A => B):Stream[B] = this match {
    case Empty => Empty
    case h :#: t =>  cons(f(h), t().map(f))
  }

  def flatMap[B](f: A => Stream[B]):Stream[B] = this match {
    case Empty => Empty
    case h :#: t => f(h) match {
      case Empty => t().flatMap(f)
      case nonEmptyStream => nonEmptyStream append (t() flatMap f)
    }
  }

  def headOption:Option[A] = this match {
    case Empty => None
    case h :#: _ => Option(h)
  }

  def tail:Stream[A] = this match {
    case Empty => Empty
    case _ :#: t => t()
  }

  def append[A1 >: A](that: => Stream[A1]):Stream[A1] = this match {
    case Empty => that
    case h :#: t => cons(h, t() append that)
  }

  def foldRight[B](z:B)(f:(A,B) => B):B = this match {
    case Empty => z
    case h :#: t => f(h, t().foldRight(z)(f))
  }

  def foldLeft[B](z:B)(f:(B,A) => B):B = this match {
    case Empty => z
    case h :#: t => t().foldLeft(f(z,h))(f)
  }

  def zip[B](other: => Stream[B]):Stream[(A,B)] = (this,other) match {
    case (Empty, _) | (_, Empty) => Empty
    case (x :#: xs, y :#: ys) => cons((x,y), xs() zip ys())
  }

  def zipWith[B,C](other: => Stream[B])(f:(A,B) => C):Stream[C] =
    zip(other).map(f.tupled)

  def find(p: A => Boolean ):Option[A] = this match {
    case Empty => None
    case h :#: t => if(p(h)) Some(h) else t() find p
  }

  def take(n:Int):Stream[A] = this match {
    case Empty => Empty
    case h :#: t => if(n <= 0) Empty else cons(h, t() take n - 1)
  }

  def toList[A1 >: A]:List[A] =
    foldRight(List.empty[A])(_ :: _)

  override def toString: String = this match {
    case Empty => "Empty"
    case h :#: _ => s"$h :#: _"
  }

}
case object Empty extends Stream[Nothing]
case class :#:[A](h: A, t: () => Stream[A]) extends Stream[A]

object Stream {

  implicit def convertOps[A1](s: => Stream[A1]):StreamOps[A1] =
    new StreamOps[A1](s)

  class StreamOps[A1](s: => Stream[A1]) {

    def :#:[A2 >: A1](a: A2): Stream[A2] =
      cons(a, s)


  }

  def empty[A]:Stream[A] =
    Empty

  def from(n:Int):Stream[Int] =
    cons(n, from(n+1))

  def cons[A](a:A, stream: => Stream[A]):Stream[A] =
    :#:(a, () => stream)

  def apply[A](as:A*):Stream[A] =
    as.foldRight(empty[A])(cons(_,_))

  def unfold[S,A](s:S)(f:S => Option[(S,A)]):Stream[A] =
    f(s) match {
      case None => empty
      case Some((nextS, a)) => cons(a, unfold(nextS)(f))
    }
}
