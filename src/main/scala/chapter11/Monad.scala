package chapter11
import chapter7.Par
import chapter8.Gen
import chapter9.Parser

abstract class Monad[F[_]] extends Functor[F] {

  def unit[A](a:A):F[A]

  def flatMap[A,B](ma:F[A])(f: A => F[B]):F[B]

  def map[A,B](ma:F[A])(f:A => B):F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma:F[A],mb:F[B])(f:(A,B) => C):F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

}

object Monad {

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

}