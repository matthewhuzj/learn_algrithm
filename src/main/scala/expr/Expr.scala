package expr

trait Expr {
  def eval: Int = this match {
    case Num(n) => n
    case Sum(l, r) => l.eval + r.eval
    case Prod(l, r) => l.eval * r.eval
  }

  def show: String = this match {
    case Num(n) => n.toString
    case Sum(l, r) => l.show + " + " + r.show
    case Prod(l, r) => parentheses(l) + " * " + parentheses(r)
  }
  def parentheses(l:Expr) :String=  l match {
    case Sum(_,_) => "( " + l.show + " )"
    case _ => l.show
  }
}

case class Sum(l:Expr,r:Expr) extends Expr
case class Prod(l:Expr,r:Expr) extends Expr
case class Num(n:Int) extends Expr

object test extends App {
//  def eval(e:Expr ) :Int = e match {
//    case Num(n) => n
//    case Sum(l,r) => eval(l) + eval(r)
//  }
  println(Prod(Sum(Num(1),Num(2)),Num(3)).eval)
  println(Prod(Sum(Num(1),Num(2)),Num(3)).show)
  println(Sum(Prod(Num(1),Num(2)),Num(3)).eval)
  println(Sum(Prod(Num(1),Num(2)),Num(3)).show)
}