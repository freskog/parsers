package chapter3

sealed trait Tree[+A] {

  def foldLeft[B](z:B)(f:(B,A) => B):B = this match {
    case     Leaf(a) => f(z,a)
    case Branch(l,r) => r.foldLeft(l.foldLeft(z)(f))(f)
  }

  def foldRight[B](z:B)(f:(A,B) => B):B = this match {
    case     Leaf(a) => f(a,z)
    case Branch(l,r) => l.foldRight(r.foldRight(z)(f))(f)
  }

}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

