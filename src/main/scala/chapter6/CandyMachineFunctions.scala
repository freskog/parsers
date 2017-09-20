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

abstract class CandyMachineFunctions[F[+_]] extends StateFunctions[Machine, F] {

  def simulateMachine(inputs:List[Input]):F[Result] =
    traverse(inputs)(a => modify(_.process(a))) *> gets(_.toResult)

}

case class CandyMachineService[+A](run: Machine => (Machine, A))

object CandyMachineService extends CandyMachineFunctions[CandyMachineService] {

  override def run[A](fa: CandyMachineService[A])(s: Machine): (Machine, A) =
    fa run s

  override def instance[A](f: (Machine) => (Machine, A)): CandyMachineService[A] =
    CandyMachineService(f)
}
