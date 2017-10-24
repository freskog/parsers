package chapter6


abstract class StateFunctions[S,F[+_]] { self =>

  def instance[A](f:S => (S,A)):F[A]

  def run[A](fa:F[A])(s:S):(S,A)

  def unit[A](a: A):F[A] =
    instance( s => (s, a))

  def flatMap[A,B](fa:F[A])(f:A => F[B]):F[B] =
    instance( s => run(fa)(s) match { case (aS, a) => run(f(a))(aS) })

  def get:F[S] =
    instance( s => (s,s))

  def put(s:S):F[Unit] =
    instance( _ => (s,()))

  def modify(f: S => S):F[Unit] =
    get map f flatMap put

  def gets[A](f: S => A):F[A] =
    get map f

  def map[A,B](fa:F[A])(f:A => B):F[B] =
    fa flatMap (f andThen unit[B])

  def map2[A,B,C](fa:F[A], fb: => F[B])(f:(A,B) => C):F[C] =
    for( a <- fa ; b <- fb ) yield f(a,b)

  def traverse[A,B](l:List[A])(f: A => F[B]):F[List[B]] =
    l.foldRight(unit[List[B]](Nil))((a, sb) => map2(f(a),sb)(_ :: _))

  def sequence[A](l:List[F[A]]):F[List[A]] =
    traverse(l)(identity)

  def dropLeft[A,B](fa:F[A],fb:F[B]):F[B] =
    fa flatMap( _ => fb)

  def dropRight[A, B](fa:F[A], fb:F[B]):F[A] =
    fa flatMap( a => fb map (_ => a))

  implicit def infix[A](fa:F[A]):FOps[A] = FOps(fa)

  case class FOps[A](fa:F[A]) {
    def flatMap[B](f: A => F[B]):F[B] = self.flatMap(fa)(f)
    def map[B](f: A => B):F[B] = self.map(fa)(f)
    def run(s:S):(S,A) = self.run(fa)(s)

    def *>[B](fb:F[B]):F[B] = self.dropLeft(fa,fb)
    def <*[B](fb:F[B]):F[A] = self.dropRight(fa,fb)
  }

}
