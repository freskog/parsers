package chapter11
import chapter12.{Applicative, Traverse}
import chapter6.State
import chapter7.Par
import chapter8.Gen
import chapter9.Parser

abstract class Monad[F[_]] extends Applicative[F] {

  def unit[A](a:A):F[A]
  def flatMap[A,B](ma:F[A])(f: A => F[B]):F[B]

  /*
   def flatMap[A,B](ma:F[A])(f: A => F[B]):F[B] =
    compose[F[A],A,B](identity, f)(ma)
  */

  def join[A](ffa:F[F[A]]):F[A] =
    flatMap(ffa)(identity)

  override def map[A,B](ma:F[A])(f:A => B):F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: => F[A],mb: => F[B])(f:(A,B) => C):F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  def compose[A,B,C](f:A => F[B],g:B => F[C]):A => F[C] =
    a => flatMap(f(a))(g)

}

object Monad {

  def composeM[F[_],G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[Lambda[x => F[G[x]]]] =
    new Monad[Lambda[x => F[G[x]]]] {
      override def unit[A](a: A): F[G[A]] =
        F.unit(G.unit(a))

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.map(F.join(F.map(ma)(ga => T.sequence[F,G[B]](G.map(ga)(f))(F))))(ggb => G.join(ggb))
    }


  import chapter5.Stream

  val genMonad:Monad[Gen] =
    new Monad[Gen] {
      override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
        ma.flatMap(f)

      override def unit[A](a: A): Gen[A] =
        Gen.unit(a)
    }

  val parMonad:Monad[Par] =
    new Monad[Par] {
      override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
        ma.flatMap(f)

      override def unit[A](a: A): Par[A] =
        Par.unit(a)
    }

  val parserMonad:Monad[Parser] =
    new Monad[Parser] {
      override def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
        ma.flatMap(f)

      override def unit[A](a: A): Parser[A] =
        Parser.succeed(a)
    }

  val optionMonad:Monad[Option] =
    new Monad[Option] {

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
        ma.flatMap(f)

      override def unit[A](a: A): Option[A] =
        Option(a)
    }

  val streamMonad: Monad[Stream] =
    new Monad[Stream] {

      override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
        ma.flatMap(f)

      override def unit[A](a: A): Stream[A] =
        Stream(a)
    }

  val listMonad: Monad[List] =
    new Monad[List] {
      override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
        ma.flatMap(f)

      override def unit[A](a: A): List[A] =
        List(a)
    }

  def eitherMonad[E]: Monad[Either[E,?]] =
    new Monad[Either[E, ?]] {
      override def unit[A](a: A):Either[E,A] =
        Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]):Either[E,B] =
        ma.flatMap(f)
    }

  def stateMonad[S]: Monad[State[S,?]] =
    new Monad[State[S,?]] {
      override def unit[A](a: A): State[S, A] =
        State.unit[S,A](a)

      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
        ma.flatMap(f)
    }

}