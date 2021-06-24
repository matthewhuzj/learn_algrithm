package scalaRiddle

object doExcise4_param extends App{
  def applyNMulti[T](n:Int)(arg:T,f:T=>T):T =
    (1 to n).foldLeft(arg) {(acc,_) => f(acc)}
  def applyNCurried[T](n:Int)(arg:T)(f:T=>T):T =
    (1 to n).foldLeft(arg) {(acc,_) => f(acc)}
  def nextInt(n:Int) = n*n+1
  def nextNumeric[N](n:N) (implicit numericOps:Numeric[N]) =
    numericOps.plus(numericOps.times(n,n),numericOps.one)

  println(applyNMulti(3)(2,nextInt))
  println(applyNCurried(3)(2)(nextInt))
  println(applyNCurried(3)(2.0)(nextNumeric))
//  println(applyNMulti(3)(2.0,nextNumeric))

}

