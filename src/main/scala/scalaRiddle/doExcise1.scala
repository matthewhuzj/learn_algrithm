package scalaRiddle

object doExcise2 extends App{
  trait A {
    val foo:Int
    val bar = 10
    println("In A bar:"+bar + ", foo:"+foo)
  }
  class B extends A {
    val foo = 25
    println("In B bar:"+bar + ", foo:"+foo)
  }
  class C extends B {
    override val bar: Int = 99
    println("In C bar:"+bar + ", foo:"+foo)
  }
  new C
}
