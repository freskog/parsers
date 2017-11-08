package chapter13

import chapter7.Par

import scala.io.StdIn
import scala.util.Try

sealed abstract class Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReader: ConsoleReader[A]
  def toState: ConsoleState[A]
}

case object ReadInt extends Console[Option[Int]] {

  override def toPar: Par[Option[Int]] =
    Par.lazyUnit(run(readInput))

  override def toThunk: () => Option[Int] =
    () => run(readInput)

  override def toReader: ConsoleReader[Option[Int]] =
    ConsoleReader(r => run(Option(r)))

  override def toState: ConsoleState[Option[Int]] =
    ConsoleState(b => (b.copy( in = if(b.in.isEmpty) Nil else b.in.tail),run(b.in.headOption)))

  def readInput:Option[String] =
    Try(StdIn.readLine("")).toOption

  def run(input:Option[String]):Option[Int] =
    input.flatMap( n => Try(n.toInt).toOption)
}

case class PrintLine(line:String) extends Console[Unit] {

  override def toPar:Par[Unit] =
    Par.lazyUnit(println(line))

  override def toThunk:() => Unit =
    () => println(line)

  override def toReader: ConsoleReader[Unit] =
    ConsoleReader(_ => ())

  override def toState: ConsoleState[Unit] =
    ConsoleState(b => (b.copy(out = if(b.out.isEmpty) Nil else b.out.tail),()))
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readInt: ConsoleIO[Option[Int]] =
    Suspend(ReadInt)

  def printLn(line:String): ConsoleIO[Unit] =
    Suspend(PrintLine(line))

}