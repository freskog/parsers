package chapter11

trait Functor[F[_]] {

  def map[A,B](fa:F[A])(f:A => B):F[B]

  def void[A](fa:F[A]):F[Unit] =
    map(fa)(_ => ())
}

object Functor {

  type Id[A] = A

  def idFunctor:Functor[Id] =
    new Functor[Id] {
      override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
    }

}