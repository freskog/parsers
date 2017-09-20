package chapter6

case class Machine(locked:Boolean, candies:Candies, coins:Coins) {

  def process(input:Input):Machine =
    input match {
      case Turn => knobTurned
      case Coin => coinInserted
    }

  def coinInserted:Machine =
    if(candies.n == 0 || !locked) this
    else copy(locked = false, coins = coins + Coins(1))

  def knobTurned:Machine =
    if(candies.n == 0 || locked) this
    else copy(locked = true, candies = candies - Candies(1))

  def toResult:Result =
    Result(coins, candies)
}

case class Coins(n:Int) extends AnyVal {
  def +(that:Coins):Coins = Coins(this.n + that.n)
}

case class Candies(n:Int) extends AnyVal {
  def -(that:Candies):Candies = Candies(this.n - that.n)
}


case class Result(coins:Coins, candies: Candies) {
  def coinInserted:Result   = copy( coins   = coins   + Coins(1))
  def candyDispensed:Result = copy( candies = candies - Candies(1))
}

object Result {
  val initial:Result = Result(Coins(0),Candies(0))
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

abstract class CandyMachineService[F[+_]] extends StateFunctions[Machine, F] {

  def simulateMachine(inputs:List[Input]):F[Result] =
    traverse(inputs)(a => modify(_.process(a))) *> gets(_.toResult)

}

case class CandyMachine[+A](run: Machine => (Machine, A))

object CandyMachineService extends CandyMachineService[CandyMachine] {

  override def run[A](fa: CandyMachine[A])(s: Machine): (Machine, A) =
    fa.run(s)

  override def unit[A](a: A): CandyMachine[A] =
    CandyMachine( m => (m, a))

  override def flatMap[A, B](fa: CandyMachine[A])(f: (A) => CandyMachine[B]): CandyMachine[B] =
    CandyMachine( m => fa.run(m) match { case (aMachine,a) => f(a).run(aMachine) } )

  override def get: CandyMachine[Machine] =
    CandyMachine( m => (m, m))

  override def put(s: Machine): CandyMachine[Unit] =
    CandyMachine( _ => (s,()))
}