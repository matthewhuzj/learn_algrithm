package scalaRiddle
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.sys.process.buildersToProcess

object Excise0 extends App{
  trait A {
    val audience: String
    println("Hello " + audience)
  }
  trait AfterA {
    val introduction: String
    println(introduction)
  }
  class BEvery(val audience: String) extends {
    val introduction = {
      println("Evaluating early def")
      "Are you there?"
    }
  } with A with AfterA {
    println("I repeat: Hello " + audience)
  }

  new BEvery({println("Evaluating param");"Readers"})
}
object Excise1 extends App {
  import collection.mutable.Buffer

  val acc1 = Buffer.empty[() => Int]
  val acc2 = Buffer.empty[() => Int]
  val data = Seq(100,200,300)
  var j = 0
  for(i<- 0 until data.size) {
    acc1 += (() => data(i))
    acc2 += (() => data(j))
    j += 1
  }
  j -=1
  acc1.foreach(a1 => println(a1()))
  acc2.foreach(a1 => println(a1()))
}
object Excise8 extends App {
  val xs = Seq(Seq("a","b","c"),Seq("d","e","f"),Seq("h","j"),Seq("x","y","z"))
  // run pass
  for (Seq(x,y,z) <- xs ) yield x + y + z
  // run fail
//  xs map { case Seq(x,y,z) => x + y + z}
  // run pass
  xs withFilter {case Seq(x,y,z) => true; case _ => false} map { case Seq(x,y,z) => x + y + z}
}