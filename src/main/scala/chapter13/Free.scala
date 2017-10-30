package chapter13

import chapter11.Monad

import scala.annotation.tailrec

sealed abstract class Free[F[_],A] {

  def flatMap[B](f: A => Free[F,B]):Free[F,B] =
    FlatMap(this, f)

  def map[B](f: A => B):Free[F,B] =
    flatMap(f andThen (Return(_)))

}
case class Return[F[_],A](a:A) extends Free[F,A]
case class Suspend[F[_],A](fa:F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](fa:Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {

  def freeMonad[F[_]:Monad]:Monad[Free[F,?]] =
    new Monad[Free[F, ?]] {
      override def unit[A](a: A): Free[F, A] =
        Return(a)

      override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
        ma.flatMap(f)
    }

  @tailrec
  def runTrampoline[A](ffa: Free[Function0,A]): A = ffa match {
    case Return(a) => a
    case Suspend(fa) => fa()
    case FlatMap(fa, f) => fa match {
      case Return(a2) => runTrampoline(f(a2))
      case Suspend(fa2) => runTrampoline(f(fa2()))
      case FlatMap(innerfa,g) => runTrampoline(innerfa flatMap (a => g(a) flatMap f))
    }
  }

  @tailrec
  def step[F[_], A](free: Free[F,A])(implicit M:Monad[F]):Free[F,A]  = free match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  /*
  def run[F[_],A](free: Free[F,A])(implicit M:Monad[F]):F[A] = step(free) match {
    case Return(a) => M.unit(a)
    case Suspend(r) => M.flatMap(r)(a => M.unit(a))
    case FlatMap(x, f) => x match {
      case Suspend(r) => M.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }*/
/*
  trait Translate[F[_],G[_]] { def apply[A](fa:F[A]):G[A] }
  type ~≳[F[_],G[_]] = Translate[F,G]

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~≳ G)( implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }
  */
}
