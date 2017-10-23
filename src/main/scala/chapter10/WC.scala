package chapter10

import chapter10.Monoid.instance
import chapter8.Gen

sealed abstract class WC {

  def count:Int = this match {
    case          Stub("") => 0
    case           Stub(_) => 1
    case Part("",words,"") => words
    case  Part("",words,_) => words + 1
    case  Part(_,words,"") => words + 1
    case   Part(_,words,_) => words + 2
  }

}

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  implicit val wc:Monoid[WC] =
    instance[WC] {
      case (Stub(lStub)             ,Stub(rStub))             => Stub(lStub + rStub)
      case (Stub(lStub)             ,Part(partL,words,rPart)) => Part(lStub + partL, words, rPart)
      case (Part(lPart,words,rPart) ,Stub(rStub))             => Part(lPart,words,rPart+rStub)
      case (Part(lp1,words1,_)      ,Part(_,words2,rp2))      => Part(lp1,words1 + words2 + 1,rp2)
    }(Stub(""))

  def genPart:Gen[Part] =
    for( s1 <- Gen.string ; i <- Gen.nonNegativeLessThan(50) ; s2 <- Gen.string ) yield Part(s1,i,s2)

  def genStub:Gen[Stub] =
    Gen.string map Stub

  def genWC:Gen[WC] =
    Gen.weighted((genPart,80.0),(genStub,20.0))

  def fromString(input:String):WC = {
    val words = input.split(" ")
    if(words.isEmpty) Stub("")
    else if(words.length == 1) Stub(words.head)
    else Part(words.head,words.length - 2, words.last)
  }

}
