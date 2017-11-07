package chapter12

import chapter10.Monoid
import chapter11.Functor
import chapter11.Functor.Id

abstract class Applicative[F[_]] extends Functor[F] { self =>

  def unit[A](a:A):F[A]
  def map2[A,B,C](fa: => F[A],fb: => F[B])(f:(A,B) => C):F[C]

  override def map[A,B](fa:F[A])(f: A => B):F[B] =
    map2(fa,unit(()))((a,_) => f(a))

  def apply[A,B](fab:F[A => B])(fa:F[A]):F[B] =
    map2(fab,fa)((f,a) => f(a))

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a,fl) => map2(f(a),fl)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if(n <= 0) unit(List.empty[A])
    else map2(ma,replicateM(n - 1, ma))(_ :: _)

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def productG[G[_]](G: Applicative[G]):Applicative[({ type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: A): (F[A], G[A]) =
        (self.unit(a),G.unit(a))

      override def map2[A, B, C](fa: => (F[A], G[A]), fb: => (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1,fb._1)(f),G.map2(fa._2,fb._2)(f))

    }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {

      override def map2[A, B, C](fa: => F[G[A]], fb: => F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa,fb)((ga,gb) => G.map2(ga,gb)(f))

      override def unit[A](a: A): F[G[A]] =
        self.unit(G.unit(a))

    }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case     Nil => unit(List.empty[A])
    case a :: as => map2(filterM(as)(f),f(a))((acc,keep) => if(keep) a :: acc else acc)
  }

  implicit def applicativeOperators[A](fa:F[A]):ApplicativeOps[A] = ApplicativeOps(fa)

  case class ApplicativeOps[A](fa:F[A]) {
    def *>[B](fb: => F[B]):F[B] = self.map2(fa,fb)((_,b) => b)
    def <*[B](fb: => F[B]):F[A] = self.map2(fa,fb)((a,_) => a)
  }
}

object Applicative {

  def apply[F[_]](implicit F:Applicative[F]):Applicative[F] = F

  implicit def validationApplicative[E]:Applicative[Validation[E,?]] =
    new Applicative[Validation[E,?]] {
      override def unit[A](a: A): Validation[E, A] =
        Success(a)

      override def map2[A, B, C](fa: => Validation[E, A], fb: => Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa,fb) match {
        case (Failure(eLeft, leftErrs),Failure(eRight,rightErrs)) => Failure[E](eRight, rightErrs ++ (eLeft +: leftErrs))
        case (fail@Failure(_, _), _ ) => fail
        case (_,fail@Failure(_,_)) => fail
        case (Success(a),Success(b)) => Success(f(a,b))
      }
    }

  implicit val idApplicative:Applicative[Id] =
    new Applicative[Id] {

      override def map2[A, B, C](fa: => Id[A], fb: => Id[B])(f: (A, B) => C): Id[C] =
        f(fa,fb)

      override def unit[A](a: A): Id[A] =
        a
    }

  type Const[M,A] = M

  implicit def monoidApplicative[M](M:Monoid[M]):Applicative[Const[M,?]] =
    new Applicative[Const[M, ?]] {

      override def map2[A, B, C](fa: => Const[M, A], fb: => Const[M, B])(f: (A, B) => C): Const[M, C] =
        M.op(fa,fb)

      override def unit[A](a: A): Const[M, A] =
        M.zero
    }
}
