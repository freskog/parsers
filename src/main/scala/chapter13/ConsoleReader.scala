package chapter13

import chapter11.Monad

case class ConsoleReader[A](run: String => A) {

  def map[B](f:A => B):ConsoleReader[B] =
    ConsoleReader(r => f(run(r)))

  def flatMap[B](f: A => ConsoleReader[B]) =
    ConsoleReader(r => f(run(r)).run(r))

}

object ConsoleReader {

  implicit val consoleReaderMonad:Monad[ConsoleReader] =
    new Monad[ConsoleReader] {
      override def unit[A](a: A): ConsoleReader[A] =
        ConsoleReader(_ => a)

      override def flatMap[A, B](ma: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] =
        ma.flatMap(f)
    }
}