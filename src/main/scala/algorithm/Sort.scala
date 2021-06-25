package algorithm

import scala.reflect.ClassTag
import scala.util.control.Breaks._

object Sort{
  def insertSort [T <% Ordered[T]](lists:List[T])(implicit ev: ClassTag[T]):List[T] ={
    if (lists.size < 2) lists
    else {
      val cpLists = lists.toArray
      for (i <- 1 until cpLists.size) {
//        cpLists.foreach(print)
//        println
        breakable {
          for (j <- i - 1 to 0 by -1) {
//            println(i,j,cpLists(j+1),cpLists(j))
            if(cpLists(j+1) < cpLists(j) ) {
              val swap =cpLists(j+1)
              cpLists(j+1) = cpLists(j)
              cpLists(j) = swap
            }
            else break
          }
        }
      }
      cpLists.toList
    }
  }
  def binarySearch[T<% Ordered[T]](array:Array[T],s:T):Int = {
    def binarySearchIter(p:Int = 0,r:Int = array.size) :Int = {
      if (p == r) p
      else {
        val q: Int = (p + r) / 2
        if (array(q) == s) q
        else if (array(q) > s) binarySearchIter(p, q - 1)
        else binarySearchIter(q + 1, r)
      }
    }
    binarySearchIter()
  }
  def mergeSort [T <% Ordered[T]](lists:List[T] )(implicit ev: ClassTag[T]):List[T] = {
    val array = lists.toArray
    mergeIter()

    def mergeIter(p:Int = 0,r:Int = array.size):Unit = {
      if (p < r -1) {
        val q:Int = (p+r)/2
        mergeIter(p,q)
        mergeIter(q,r)
        merge(p,q,r)
      }
    }
    def merge(p:Int,q:Int,r:Int):Unit = {
      for(i <- q until r if array(i) < array(i-1)) {
        val tmp = array(i)
        for (j <- i until p by -1 if array(j) < array(j-1)) {
          array(j) = array(j-1)
          array(j-1) = tmp
        }
      }
    }
    array.toList
  }

}

object testSort extends App {
  common.RunTime.time {println(Sort.insertSort(List(3,2,6,4,7,5,1,0)))}
  common.RunTime.time {println(Sort.mergeSort(List(3,2,6,4,7,5,1,0)))}
  common.RunTime.time {println(Sort.binarySearch(Array(0,2,12,33,89,123,200,201,202,399),5))}
  common.RunTime.time {println(Sort.binarySearch(Array(0,2,12,33,89,123,200,201,202,399),399))}
  common.RunTime.time {println(Sort.binarySearch(Array(0,2,12,33,89,123,200,201,202,399),0))}

}