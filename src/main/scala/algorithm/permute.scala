package algorithm

object permute {
  def permute(str:String):Unit = {
    val array = str.toArray
    val len = array.size

    def permuteIter(arr1:Array[Char],a:Int = 0):Unit = {
      val arr = arr1.clone()
      for (i<- a until len) {
        val tmp = arr(a)
        arr(a) = arr(i)
        arr(i) = tmp
        permuteIter(arr,i+1)
        print(i,a," ")
      }
      arr.foreach(print)
      println
    }
    permuteIter(array)

  }

}

object test1 extends App{
  permute.permute("ABC")
}