package chapter12

import chapter10.{Foldable, Monoid}
import chapter11.{Functor, Monad}
import chapter11.Functor.Id
import chapter12.Applicative.Const
import chapter6.State

trait Traverse[F[_]] extends Foldable[F] with Functor[F] { self =>

  override def foldMap[A,M](as: F[A])(f: A => M)(mb:Monoid[M]):M =
    traverse[Const[M,?],A,Nothing](as)(f)(mb)

  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])( implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_],A](fga: F[G[A]])( implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Applicative[Id].unit(f(a)))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, ?],A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (S, F[B]) =
    traverseS(fa)(a =>
      for {
            s1 <- State.get[S]
        (b,s2) = f(a,s1)
             _ <- State.put[S](s2)
      } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._1.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._2

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum[B,A,Unit](as,z)((a,b) => ((),f(b,a)))._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._2

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    (traverse(fa)(f)(G),traverse(fa)(g)(H))

  def compose[G[_]]( implicit G: Traverse[G]): Traverse[Lambda[x => F[G[x]]]] =
    new Traverse[Lambda[x => F[G[x]]]] {
      override def traverse[H[_], A, B](fga: F[G[A]])(f: A => H[B])(implicit H: Applicative[H]): H[F[G[B]]] =
        self.traverse[H,G[A],G[B]](fga)(ga => G.traverse(ga)(a => f(a)))
    }

}

object Traverse {
  implicit val function0Traverse:Traverse[Function0] = new Traverse[Function0] {
    override def traverse[G[_], A, B](fa: () => A)(f: A => G[B])(implicit G: Applicative[G]): G[() => B] =
      G.map(f(fa()))(b => () => b)
  }
}