package unrelated

sealed abstract class BTree[A : Ordering] {

  import scala.math.Ordering.Implicits._

  def put(a:A):BTree[A] = this match {
    case Empty() => Node(a,Empty(),Empty())
    case Node(na,l,r) => if(a < na) Node(na,l.put(a),r) else if(a > na) Node(na,l,r.put(a)) else Node(a,l,r)
  }

  def delete(a:A):BTree[A] = this match {
    case Empty() => Empty()
    case Node(na, l, r) => if(na == a) l.merge(r) else Node(na,l,r)
  }

  def foldLeft[B](z:B)(f:(B,A) => B):B = this match {
    case Empty() => z
    case Node(a,l,r) => r.foldLeft(f(l.foldLeft(z)(f),a))(f)
  }

  def merge(b:BTree[A]):BTree[A] =
    b.foldLeft(this)(_ put _)

  def map[B : Ordering](f:A => B):BTree[B] =
    foldLeft(BTree.empty[B])((acc,a) => acc.put(f(a)))

  def flatMap[B : Ordering](f: A => BTree[B]):BTree[B] =
    foldLeft(BTree.empty[B])((acc, a) => acc.merge(f(a)))

}

object BTree {

  def empty[A : Ordering]:BTree[A] = Empty()

}

case class Node[A : Ordering](a:A,left:BTree[A],right:BTree[A]) extends BTree[A]
case class Empty[A : Ordering]() extends BTree[A]