package unrelated

object Combinations {

  def powerSet[A](l:List[A]):List[List[A]] =
    if(l.isEmpty) List(List.empty[A])
    else merge(l.head, powerSet(l.tail))

  def merge[A](a: A, l: List[List[A]]): List[List[A]] =
    l ::: l.map(a :: _)

  def permutations[A](l:List[A]):List[List[A]] =
    l.foldRight(List(List.empty[A]))((a, acc) => acc.flatMap(insertEveryWhere(a,_)))

  def insertEveryWhere[A](a:A,l:List[A]):List[List[A]] =
    if(l.isEmpty) List(List(a))
    else (a :: l) :: insertEveryWhere(a, l.tail).map( l.head :: _ )

}
