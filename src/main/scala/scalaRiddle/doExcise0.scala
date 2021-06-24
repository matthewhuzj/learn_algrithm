package scalaRiddle

import scala.math.{abs, exp}

object doExcise0 extends App{
  def mapReduce(f:Int => Int,combine:(Int,Int) => Int ,zero:Int)(a:Int,b:Int):Int =
    if(a > b) zero
    else combine(f(a),mapReduce(f,combine,zero)(a+1,b))

  def product(f:Int=>Int)(a:Int,b:Int) = mapReduce(f,(x,y)=>x*y,1)(a,b)

  def fact(n:Int) = product(x=>x)(1,n)

  def fact1(n:Int) = mapReduce(x=>x,(x,y)=>x*y,1)(1,n)
  println(fact(5))
  println(fact1(5))
}

object doExcise1 extends App {
  val tolerance = 0.001
  def isCloseEnough(x:Double,y:Double):Boolean =
    abs((x-y)/x) < tolerance
  def fixedPoint(f:Double => Double) (firstGuess:Double):Double = {
    def nextIter(guess:Double) :Double = {
      val next = f(guess)
      if (isCloseEnough(guess,next)) next
      else nextIter(next)
    }
    nextIter(firstGuess)
  }
  //dampen average
  def dampenAverage(f:Double=>Double)(x:Double)  = (x+f(x))/2

  def sqrt(x:Double) = fixedPoint(dampenAverage( y => x/y))(1)
  def trip(x:Double) = fixedPoint(dampenAverage(y => x/(y*y)))(1)
  println(sqrt(2))
  println(trip(8))
}
