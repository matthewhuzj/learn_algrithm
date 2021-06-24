package scalaRiddle

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def pascalIter(acc:Int,c:Int,r:Int) :Int = {
      if( c == 0 || c == r) acc +1
      else pascalIter(0,c-1,r-1) +pascalIter(0,c,r-1)
    }
    pascalIter(0,c,r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(leftP:Int,chars:List[Char]):Boolean = {
      if ((chars.isEmpty && leftP != 0) || leftP < 0) false
      else if(chars.isEmpty && leftP == 0 ) true
      else {
        if (chars.head == '(') balanceIter(leftP + 1, chars.tail)
        else if(chars.head == ')') balanceIter(leftP - 1, chars.tail)
        else balanceIter(leftP, chars.tail)
      }
    }
    balanceIter(0,chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val newCoins = coins.sortWith(_>_)
    def countChangeIter(n:Int,total:Int,coins:List[Int]) :Int = {
      if (coins.isEmpty || total > money ) n
      else if(total == money) n+1
      else countChangeIter(n,total+coins.head,coins) +countChangeIter(n,total,coins.tail)
    }
    countChangeIter(0,0,newCoins)
  }
}
