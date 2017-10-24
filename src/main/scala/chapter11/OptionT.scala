package chapter11

case class OptionT[M[_],A](value:M[Option[A]])(implicit M:Monad[M]) {

  def flatMap[B](f: A => OptionT[M, B]):OptionT[M,B] =
    OptionT(M.flatMap(value) {
      case    None => M.unit(None)
      case Some(a) => f(a).value
    })


}