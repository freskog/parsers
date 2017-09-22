package chapter7

import java.util.concurrent.{Callable, ExecutorService, TimeUnit}

abstract class ParFunctions[F[+_]] { self =>

  def unit[A](a:A):F[A]

  def flatMap[A,B](fa:F[A])(f:A => F[B]):F[B]

  def fork[A](fa: => F[A]):F[A]

  def map2[A,B,C](fa:F[A],fb:F[B])(f:(A,B) => C):F[C]

  def map[A,B](fa:F[A])(f: A => B):F[B] =
    fa flatMap(f andThen unit)

  def lazyUnit[A](a: => A): F[A] =
    fork(unit(a))

  def asyncF[A,B](f: A => B): A => F[B] =
    (a:A) => lazyUnit(f(a))

  def parMap[A,B](l:List[A])(f: A => B): F[List[B]] =
    fork(sequence(l.map(asyncF(f))))

  def parFilter[A](as:List[A])(f:A => Boolean):F[List[A]] =
    if(as.isEmpty) unit(List.empty[A])
    else parFilter(as.tail)(f).map( l => if(f(as.head)) as.head :: l else l)

  def sequence[A](l:List[F[A]]):F[List[A]] =
    l.foldRight(unit(List.empty[A]))((fa,listOfFa) => fa flatMap (a => listOfFa.map( a :: _)))

  implicit def operators[A](fa:F[A]):ParOps[A] =
    ParOps(fa)

  case class ParOps[A](fa:F[A]) {
    def flatMap[B](f:A => F[B]):F[B] = self.flatMap(fa)(f)
    def map[B](f:A => B):F[B] = self.map(fa)(f)
  }
}


case class UnitFuture[A](a:A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean):Boolean = false
  override def isCancelled:Boolean = false
  override def isDone:Boolean = true
  override def get[A1 >: A]():A1 = a
  override def get[A1 >: A](timeout: Long, unit: TimeUnit):A1 = a
}

case class TandemFuture[A,B,+C](fa:Future[A], fb:Future[B], f: (A,B) => C) extends Future[C] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean =
    fa.cancel(mayInterruptIfRunning) && fb.cancel(mayInterruptIfRunning)

  override def isCancelled: Boolean =
    fa.isCancelled || fb.isCancelled

  override def isDone:Boolean =
    fa.isDone && fb.isDone

  override def get[C1 >: C]():C1 = f(fa.get(), fb.get())

  override def get[C1 >: C](timeout: Long, unit: TimeUnit):C1 = {
    val timeoutInMs = TimeUnit.MILLISECONDS.convert(timeout, unit)
    val startedAt = System.currentTimeMillis()
    val a = fa.get(timeout, unit)
    val remainingTime = timeoutInMs - (System.currentTimeMillis() - startedAt)
    val b = fb.get(remainingTime, TimeUnit.MILLISECONDS)
    f(a,b)
  }
}

trait Future[+A] {
  def cancel(mayInterruptIfRunning: Boolean):Boolean
  def isCancelled:Boolean
  def isDone:Boolean
  def get[A1 >: A]():A1
  def get[A1 >: A](timeout: Long, unit: TimeUnit):A1
}

object Future {

  implicit def fromJavaFuture[A](f:java.util.concurrent.Future[A]):Future[A] =
    new Future[A] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        f.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean =
        f.isCancelled

      override def isDone: Boolean =
        f.isDone

      override def get[A1 >: A](): A1 =
        f.get()

      override def get[A1 >: A](timeout: Long, unit: TimeUnit): A1 =
        f.get(timeout, unit)
    }
}

case class Par[+A](run:ExecutorService => Future[A])

object Par extends ParFunctions[Par] {

  override def unit[A](a: A):Par[A] =
    Par((_: ExecutorService) => UnitFuture(a))

  override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]):Par[B] =
    Par((e: ExecutorService) => f(fa.run(e).get()).run(e))

  override def fork[A](fa: => Par[A]):Par[A] =
    Par((e: ExecutorService) => e.submit(new Callable[A] { override def call(): A = fa.run(e).get() }))

  override def map2[A, B, C](fa: Par[A], fb: Par[B])(f: (A, B) => C): Par[C] =
    Par((e: ExecutorService) => TandemFuture(fa.run(e),fb.run(e),f))
}