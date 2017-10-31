package chapter3

import scala.annotation.tailrec

abstract class Sortable[F[_]] {

  def sort[A : Ordering](fa:F[A]):F[A]

}

object Sortable {

  def apply[F[_]](implicit S:Sortable[F]):Sortable[F] = S

  implicit val mergeSort:Sortable[IndexedSeq] =
    new Sortable[IndexedSeq] {

      import scala.math.Ordering.Implicits._

      @tailrec
      final def merge[A : Ordering](s1:IndexedSeq[A],s2:IndexedSeq[A],acc:IndexedSeq[A]):IndexedSeq[A] =
             if(s1.isEmpty)         acc ++ s2
        else if(s2.isEmpty)         acc ++ s1
        else if(s1.head <= s2.head) merge(s1.tail, s2     , acc :+ s1.head)
        else                        merge(s1     , s2.tail, acc :+ s2.head)

      def sort[A : Ordering](seq:IndexedSeq[A]):IndexedSeq[A] =
        if(seq.length < 2) seq
        else {
          val (l, r) = seq.splitAt(seq.size / 2)
          merge(sort(l), sort(r), IndexedSeq.empty[A])
        }
    }

}
