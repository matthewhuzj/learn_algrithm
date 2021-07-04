package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = {x:Int => x == elem}


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = {x:Int => s(x) | t(x)}

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = {x:Int => s(x) & t(x)}

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = {x:Int => s(x)^t(x)}

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = {
    x:Int => if(p(x)) s(x) else false
  }


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s,a) & !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s,p)

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = {
    x:Int => if (contains(s,f(x))) s(f(x)) else false
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }

  /**
   * Generate prime.
   */
  def genPrime():FunSet = {
    import math.{sqrt,floor}
    val prime = singletonSet(2)
    val pick:FunSet  = x => (x > 1) & (x % 2 == 1)
    val maxSqrtBound = floor(sqrt(bound)).toInt
//    for (i<- 3 to maxSqrtBound) {
//      if (contains(pick,i)) {
//        printSet(prime)
//        printSet(pick)
//        prime = union(prime,singletonSet(i))
//        pick = intersect(pick,x => x % i != 0)
//      }
    def primeIter(i:Int,p:FunSet,r:FunSet):FunSet = {
      if(i > maxSqrtBound) union(p,r)
      else if(contains(r,i)) {
        val prime = union(p,singletonSet(i))
        val pick = intersect(r,x=> x % i != 0)
        primeIter(i+1,prime,pick)
      } else {
        primeIter(i+1,p,r)
      }
    }
    primeIter(3,prime,pick)
  }
}

object FunSets extends FunSets

object testUnit extends App {
  FunSets.printSet(FunSets.genPrime())
}