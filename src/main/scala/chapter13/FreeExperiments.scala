package chapter13


import chapter11._

object FreeExperiments {

  import Console._,Free._, Monad._

  /**
    * This can be used to test if the free implementation is stack safe.
    *
    * Interpreting from ConsoleIO to ConsoleReader :
    *
    * Free.runConsoleFunction0(FreeExperiments.printN("",100000))() // This will blow up
    * Free.runStackSafeConsole(FreeExperiments.printN("",100000))   // will print many empty lines
    *
    */

  def printN(str:String,n:Int):ConsoleIO[Unit] =
    if(n <= 0) Monad[ConsoleIO].unit(())
    else printLn(str) <* printN(str, n - 1)

}
