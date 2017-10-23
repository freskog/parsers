package chapter10

import chapter10.Monoid.instance

sealed abstract class Comparison[A] {
  def asc(that:Comparison[A])(implicit ordered: Ordering[A]):Comparison[A] = (this,that) match {
    case (left      , Identity())  => left
    case (Identity(), right)       => right
    case (Asc(n1)   , Asc(n2))     => if(Ordering[A].lteq(n1, n2)) Asc(n2) else NotOrdered()
    case _                         => NotOrdered()
  }

  def desc(that:Comparison[A])(implicit ordered: Ordering[A]):Comparison[A] = (this,that) match {
    case (left       , Identity()) => left
    case (Identity() , right)      => right
    case (Desc(n1)   , Desc(n2))   => if(Ordering[A].lteq(n1, n2)) Asc(n2) else NotOrdered()
    case _                         => NotOrdered()
  }

}

case class Asc[A](last: A) extends Comparison[A]
case class Desc[A](last: A) extends Comparison[A]
case class Identity[A]() extends Comparison[A]
case class NotOrdered[A]() extends Comparison[A]

object Comparison {

  implicit def ascending[A : Ordering]:Monoid[Comparison[A]] =
    instance[Comparison[A]](_ asc _)(Identity())

  implicit def descending[A : Ordering]:Monoid[Comparison[A]] =
    instance[Comparison[A]](_ desc _)(Identity())

}