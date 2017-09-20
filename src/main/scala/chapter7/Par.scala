package chapter7

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

abstract class ParFunctions[F[+_]] { self =>

  def run[A](fa: F[A])(e:ExecutorService):A

  def unit[A](a:A):F[A]

  def flatMap[A,B](fa:F[A])(f:A => F[B]):F[B]

  def fork[A](a: => F[A]):F[A]

  def map2[A,B,C](fa:F[A],fb:F[B])(f:(A,B) => C):F[C]

  def map[A,B](fa:F[A])(f: A => B):F[B] =
    fa flatMap(f andThen unit)

  def lazyUnit[A](a: => A): F[A] =
    fork(unit(a))

  implicit def operators[A](fa:F[A]):ParOps[A] =
    ParOps(fa)

  case class ParOps[A](fa:F[A]) {
    def flatMap[B](f:A => F[B]):F[B] = self.flatMap(fa)(f)
    def map[B](f:A => B):F[B] = self.map(fa)(f)
  }
}

case class Par[+A](f:ExecutorService => Future[A])
case class UnitFuture[A](a:A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean):Boolean =
    false

  override def isCancelled:Boolean =
    false

  override def isDone:Boolean =
    true

  override def get():A =
    a

  override def get(timeout: Long, unit: TimeUnit):A =
    a
}

object Par extends ParFunctions[Par] {
  override def run[A](fa: Par[A])(e:ExecutorService):A =
    fa.f(e).get()

  override def unit[A](a: A):Par[A] =
    Par((_: ExecutorService) => UnitFuture(a))

  override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]):Par[B] =
    Par((e: ExecutorService) => f(run(fa)(e)).f(e))

  override def fork[A](fa: => Par[A]):Par[A] =
    Par((e: ExecutorService) => fa.f(e))

  override def map2[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    Par((e: ExecutorService) => {
      val af: Future[A] = fa.f(e)
      val bf: Future[B] = fb.f(e)
      UnitFuture(f(af.get,bf.get))
    })
}