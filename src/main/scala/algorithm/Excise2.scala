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
  def maxArray(array: Array[Int]): Array[Int]={
    def maxArrayIter(low:Int = 0,high:Int = array.size):Array[Int] = {
      if(low +1 == high) Array(low,high,array(low))
      else {
        val mid = (low+high)/2
        val leftA = maxArrayIter(low,mid)
        val rightA = maxArrayIter(mid,high)
        val crossA = crossArray(low,mid,high)
        if (leftA(2) >= rightA(2) && leftA(2) >= crossA(2)) leftA
        else if(leftA(2) < rightA(2) && rightA(2) >= crossA(2)) rightA
        else crossA
      }
    }
    def  crossArray(low:Int ,mid:Int,high:Int ):Array[Int] = {
      var left_sum = array(mid)
      var sum = array(mid)
      var max_left = mid
      for (i<- mid-1 to low by -1) {
        sum = sum + array(i)
        if (sum > left_sum ) {
          left_sum = sum
          max_left = i
        }
      }
      var right_sum = 0
      sum = 0
      var max_right = mid+1
      for (i <- mid +1 until high) {
        sum = sum + array(i)
        if (sum > right_sum ) {
          right_sum = sum
          max_right = i+1
        }
      }
      Array(max_left,max_right,left_sum+right_sum)
    }
    maxArrayIter()
  }
}

object testCase extends App {
  time {println(Excise2.insertSort(List(3,6,4,5,1,2)))}
  time {Excise2.maxArray(Array(-1,-2,-3,4,-3,-2,-1)).foreach(print)}
  println
  time {Excise2.maxArray(Array(-1,8,-3,4,-3,-2,-1)).foreach(print)}
  println
  time {Excise2.maxArray(Array(-1,-8,-3,-4,-3,-2,-9)).foreach(print)}
  println
  time {Excise2.maxArray(Array(1,8,3,4,3,2,9)).foreach(print)}
  println
}