package chapter10


abstract class Foldable[F[_]] {

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B

  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
    foldMap(as)(a => f(a,_))(Monoid.endo[B])(z)

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

  def fromFoldLeft[F[_],A,B](f:(F[A],B, (B => A)) => B):Foldable[F] =
    new Foldable[F] {
      override def foldLeft[AA, BB](as: F[AA])(z: BB)(f: (BB, AA) => BB): BB =
        f(as,z,f)
    }
}

abstract class FoldableInstances {

  import chapter5.Stream
  import chapter3.Tree

  implicit def list[A]:Foldable[List] =
    Foldable.fromFoldLeft(_.foldLeft(_)(_))

  implicit def indexedSeq[A]:Foldable[IndexedSeq] =
    Foldable.fromFoldLeft(_.foldLeft(_)(_))

  implicit def stream[A]:Foldable[Stream] =
    Foldable.fromFoldLeft(_.foldLeft(_)(_))

  implicit def tree[A]:Foldable[Tree] =
    Foldable.fromFoldLeft(_.foldLeft(_)(_))

  implicit def option[A]:Foldable[Option] =
    Foldable.fromFoldLeft(_.fold(_)(_))

}