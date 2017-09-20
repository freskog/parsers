package chapter6

import scala.math.{abs, max}

abstract class RNG {
  def nextInt:(RNG, Int)
}


case class SimpleRNG(seed:Long) extends RNG {
  override def nextInt: (RNG,Int) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (nextRNG, n)
  }
}

abstract class RandFunctions[F[+_]] extends StateFunctions[RNG, F] { self =>

  def int:F[Int] =
    get map (_.nextInt) flatMap { case (rng, i) => put(rng).map(_ => i) }

  def nonNegativeInt:F[Int] =
    int map abs map (max(_, 0))

  def double:F[Double] =
    nonNegativeInt map (_ - 1) map (max(_, 0)) map (_.toDouble / Int.MaxValue)

  def ints(count:Int):F[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeEven:F[Int] =
    nonNegativeInt map ( i => i - i % 2)

  def nonNegativeLessThan(n:Int):F[Int] =
    for {
      i <- nonNegativeInt
      result <- if(i + (n-1) - (i % n) >= 0) unit(i % n) else nonNegativeLessThan(n)
    } yield result
}

case class Rand[+A](run: RNG => (RNG, A))

object Rand extends RandFunctions[Rand] {
  override def run[A](fa: Rand[A])(s: RNG): (RNG, A) =
    fa.run(s)

  override def unit[A](a: A): Rand[A] =
    Rand( rng => (rng, a))

  override def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] =
    Rand( rng => fa.run(rng) match { case (aRng, a) => f(a).run(aRng) })

  override def get: Rand[RNG] =
    Rand( rng => (rng, rng) )

  override def put(s: RNG): Rand[Unit] =
    Rand( _ => (s, ()))
}