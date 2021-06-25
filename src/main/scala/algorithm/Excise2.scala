package algorithm
import common.RunTime.time

import scala.reflect.ClassTag

object Excise2 {
  def insertSort[T <% Ordered[T]](lists:List[T])(implicit ev:ClassTag[T]):List[T] = {
    val cpLists = lists.toArray
    if (lists.size < 2) lists
    else {
      for(i <- 2 until lists.size) {
        val tmp = cpLists(i)
        for(j <- i-1 to 0 by -1 if (tmp < cpLists(j))) {
          cpLists(j+1) = cpLists(j)
          cpLists(j) = tmp
        }
      }
      cpLists.toList
    }
  }
  def maxArray[T <: Double](array: Array[T]):Array[Int] ={
    def maxArrayIter(low:Int = 0,high:Int = array.size):Array[Int] = {
      if(low +1 == high) Array(low,high,array(low))
      else {
        val mid = (low+high)/2
        val leftA = maxArrayIter(low,mid)
        val rightA = maxArrayIter(mid,high)
        val crossA = crossArray(low,mid,high)
        Array(1)
      }

    }
    def  crossArray(low:Int ,mid:Int,high:Int ):Array[Int] = {
      var left_sum = -1

      Array(1)
    }

    Array(1)

  }
}

object testCase extends App {
  time {println(Excise2.insertSort(List(3,6,4,5,1,2)))}
}