package scalaRiddle

object doExcise3_set extends App{
  def sumSizesFail(collections:Iterable[Iterable[_]]) :Int =
    collections.map(_.size).sum
  def sumSizes(collections:Iterable[Iterable[_]]) :Int =
    collections.toSeq.map(_.size).sum
  def sumSizes1(collections:Iterable[Iterable[_]]) :Int = {
    collections.foldLeft(0) {
      (sumSizesOf,collection) => sumSizesOf + collection.size
    }
  }
  println(sumSizesFail(List(List(1,2),Set(3,4))))
  println(sumSizesFail(Set(List(1,2),Set(3,4))))
  println(sumSizes(List(List(1,2),Set(3,4))))
  println(sumSizes(Set(List(1,2),Set(3,4))))
  println(sumSizes1(List(List(1,2),Set(3,4))))
  println(sumSizes1(Set(List(1,2),Set(3,4))))
}
