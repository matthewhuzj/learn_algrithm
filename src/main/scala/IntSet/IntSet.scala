package IntSet

abstract class IntSet {
  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
  def union(other:IntSet):IntSet

}

class Empty extends IntSet {
  def incl(x:Int) =new NonEmpty(x,new Empty,new Empty)
  def contains(x: Int): Boolean = false
  override def union(other: IntSet): IntSet = other
  override def toString = "."
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x:Int):IntSet = {
    if(x < elem) new NonEmpty(elem,left incl x,right)
    else if(x > elem) new NonEmpty(elem,left,right incl x)
    else this
  }

  def contains(x: Int): Boolean = {
    if(x < elem) this.left.contains(x)
    else if(x > elem) this.right.contains(x)
    else true
  }

  def union(other:IntSet) :IntSet  = {
    ((left union right) union other) incl elem
  }

  override def toString: String = {
    "{" + left + elem + right +"}"
  }
}