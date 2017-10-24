package chapter10


abstract class Foldable[F[_]] {

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B

  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
    foldMap[A,B => B](as)(a => f(a,_))(Monoid.endo[B])(z)

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b,a) => mb.op(b,f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa:F[A]):List[A] =
    foldRight[A,List[A]](fa)(List.empty[A])(_ :: _)

}

object Foldable extends FoldableInstances {

  @inline
  def apply[F[_]](implicit F:Foldable[F]):Foldable[F] = F


}

abstract class FoldableInstances {

  import chapter5.Stream
  import chapter3.Tree

  implicit def list:Foldable[List] =
    new Foldable[List] {
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
    }

  implicit def indexedSeq:Foldable[IndexedSeq] =
    new Foldable[IndexedSeq] {
      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
    }

  implicit def stream:Foldable[Stream] =
    new Foldable[Stream] {
      override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B):B =
        as.foldLeft(z)(f)
    }

  implicit def tree:Foldable[Tree] =
    new Foldable[Tree] {
      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
    }

  implicit def option:Foldable[Option] =
    new Foldable[Option] {
      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
        as.foldLeft(z)(f)
    }

}