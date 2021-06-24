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
}

object testSort extends App {
  RunTime.time {println(Sort.insertSort(List(3,2,6,4,7,5,1,0)))}
}