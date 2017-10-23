package chapter10

import chapter10.Monoid.instance
import chapter6.SimpleRNG
import chapter7.Par
import chapter8.{Gen, Prop}

trait Monoid[A] {

  def op(a1:A, a2:A):A
  def zero:A

}

object Monoid extends MonoidInstances {

  @inline
  def apply[A](implicit M:Monoid[A]):Monoid[A] = M

  def instance[A](f:(A,A) => A)(z:A):Monoid[A] =
    new Monoid[A] {
      override def op(a1: A, a2: A): A = f(a1,a2)
      override def zero: A = z
    }

  def foldMap[A,B](list:List[A], m:Monoid[B])(f:A => B):B =
    list.foldLeft(m.zero)((acc,a) => m.op(acc,f(a)))


  def foldMapV[A,B](v: IndexedSeq[A], m:Monoid[B])(f:A => B):B =
    if(v.isEmpty) m.zero
    else if(v.length == 1) f(v.head)
    else v.splitAt(v.length / 2) match {
      case (left, right) => m.op(foldMapV(left,m)(f), foldMapV(right, m)(f))
    }

  def par[A](m:Monoid[A]):Monoid[Par[A]] =
    instance[Par[A]]((pa1,pa2) => Par.map2(pa1,pa2)(m.op))(Par.unit(m.zero))

  def parFoldMap[A,B](v:IndexedSeq[A],m:Monoid[B])(f: A => B):Par[B] =
    if(v.isEmpty) par(m).zero
    else if(v.length == 1) Par.unit(f(v.head))
    else v.splitAt(v.length / 2) match {
      case (left, right) => par(m).op(parFoldMap(left,m)(f),parFoldMap(right,m)(f))
    }
}

trait MonoidInstances {

  val string: Monoid[String] =
    instance[String](_ + _)("")

  def list[A]: Monoid[List[A]] =
    instance[List[A]](_ ::: _)(Nil)

  val intAdd: Monoid[Int] =
    instance[Int](_ + _)(0)

  val intMul: Monoid[Int] =
    instance[Int](_ * _)(1)

  val booleanOr: Monoid[Boolean] =
    instance[Boolean](_ || _)(false)

  val booleanAnd: Monoid[Boolean] =
    instance[Boolean](_ && _)(true)

  def option[A: Monoid]: Monoid[Option[A]] =
    instance[Option[A]]((o1, o2) => o1.flatMap(a1 => o2.map(a2 => Monoid[A].op(a1, a2))))(Option(Monoid[A].zero))

  def endo[A]: Monoid[A => A] =
    instance[A => A](_ andThen _)(identity)

}


trait MonoidLaws {
  def one[A : Gen](f:A => Boolean):Prop =
    Gen.forAll[A](Gen[A])(f)

  def three[A : Gen](f:(A,A,A) => Boolean):Prop =
    Gen.forAll[(A,A,A)](for (a1 <- Gen[A] ; a2 <- Gen[A] ; a3 <- Gen[A]) yield (a1,a2,a3))(f.tupled)

  def rightIdentity[A : Gen](m:Monoid[A]):Prop =
    one[A](a => m.op(a,m.zero) == a)

  def leftIdentity[A : Gen](m:Monoid[A]):Prop =
    one[A](a => m.op(m.zero,a) == a)

  def associative[A : Gen](m:Monoid[A]):Prop =
    three[A]((a1,a2,a3) => m.op(m.op(a1,a2),a3) == m.op(a1, m.op(a2,a3)))

  def monoidLaws[A : Gen](m:Monoid[A]):Prop =
    rightIdentity(m) && leftIdentity(m) && associative(m)
}

object MonoidTest extends MonoidInstances with MonoidLaws {
  def test(seed:Long):Unit = {
    val rng = SimpleRNG(seed)
    println(s"int add : ${monoidLaws(intAdd)(Gen.int).check(0, rng)}")
    println(s"int mul : ${monoidLaws(intMul)(Gen.int).check(0, rng)}")
    println(s"bool and : ${monoidLaws(booleanAnd)(Gen.boolean).check(0, rng)}")
    println(s"bool or : ${monoidLaws(booleanAnd)(Gen.boolean).check(0, rng)}")
    println(s"option int : ${monoidLaws(option(intAdd))(Gen.optionOf(Gen.int)).check(0, rng)}")
    println(s"endo add int : ${monoidLaws(endo[Int])(Gen.unit[Int => Int](_ + 1)).check(0, rng)}")
    println(s"wc : ${monoidLaws(wc)(WC.genWC).check(0,rng)}")
  }
}