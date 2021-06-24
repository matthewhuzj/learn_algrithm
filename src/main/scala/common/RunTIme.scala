package common

object RunTime {
  def time[R] (block : => R) :R = {
    val start = System.currentTimeMillis()
    val res   = block
    val end   = System.currentTimeMillis()
    println("\n"+"-"*40)
    println("Past:"+(end-start)+"ms")
    println("-"*40)
    res
  }
}
