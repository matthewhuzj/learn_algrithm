package rational


class Rational(x:Int,y:Int) extends Comparable[Rational]{
  require(y != 0 )
  private def gcd(a:Int,b:Int):Int = if( b == 0 ) a else gcd(b,a%b)
  val g = gcd(x,y)
  val nomer = x/g
  val denom = y/g

  def this(x:Int) = this(x,1)

  def < (that:Rational) :Boolean = {
    this.nomer * that.denom < that.nomer * this.denom
  }

  def == (that:Rational) : Boolean = {
    this.nomer * that.denom == that.nomer * this.denom
  }

  def > (that:Rational) :Boolean = this.nomer * that.denom > that.nomer * this.denom

  def + (that:Rational) : Rational = {
    new Rational(this.nomer * that.denom + that.nomer * this.denom , this.denom * that.denom)
  }

  def unary_- : Rational = new Rational(-this.nomer,this.denom)

  def - (that:Rational):Rational = this + -that

  def * (that:Rational) : Rational = new Rational(this.nomer * that.nomer,this.denom * that.denom)

  def / (that:Rational) : Rational = new Rational(this.nomer * that.denom,this.denom * that.nomer)


  override def  toString() ={
    if (denom == 1) nomer.toString
    else nomer + "/" + denom
  }

  override def compareTo(o: Rational): Int = {
    if (this < o)  -1
    else if (this == o) 0
    else 1
  }

}


object Rational extends App {
  implicit def IntToRational(x:Int) :Rational = new Rational(x)
  val a = new Rational(1,2)
  val b = new Rational(2,3)
  println(a+b)
  println(a * 2)
  println(2 * a)
}