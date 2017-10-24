package chapter11

abstract class Functor[F[_]] {

  def map[A,B](fa:F[A])(f:A => B):F[B]

  def void[A](fa:F[A]):F[Unit] =
    map(fa)(_ => ())
}
