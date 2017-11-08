package chapter13

import chapter11.Monad

case class ConsoleState[A](run:Buffers => (Buffers, A)) {

  def flatMap[B](f: A => ConsoleState[B]):ConsoleState[B] =
    ConsoleState( buffers => run(buffers) match {
      case (updatedBuffers, a) => f(a).run(updatedBuffers)
    })
}

object ConsoleState {
  implicit val consoleStateMonad:Monad[ConsoleState] =
    new Monad[ConsoleState] {
      override def unit[A](a: A): ConsoleState[A] =
        ConsoleState(b => (b, a))

      override def flatMap[A, B](ma: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] =
        ma.flatMap(f)
    }
}

case class Buffers(in:List[String],out:List[String])

