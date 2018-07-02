package funsets
import sun.font.TrueTypeFont


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = {
    s: Int => if (elem == s) true else false
  }


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = {
    // either being true is good enough.
    x: Int => s(x) || t(x)
  }

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = {
    // needs to be true for both
    x: Int => s(x) && t(x)
  }

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = {
    // return anything where it exists in `s` and not `t`
    x: Int =>  s(x) && !t(x)
  }

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = {
    // a filter and intersection are the same
    intersect(s, p)
  }

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      // if we have iterated through the entire set and reached the ubound, assume all bounded ints are true
      if (a > bound) true
      // if we find an exception then exit the loop, because all are not conforming to set p
      else if (contains(s,a) && !intersect(s, p)(a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      // if we have iterated through the entire set and reached the ubound, assume nothing was found.
      if (a > bound) false
      // if we found anything just return true, no need to evaluate any more.
      else if (contains(s,a) && intersect(s, p)(a)) true
      else iter(a + 1)
  }
  iter(-bound)
}
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def newMap(x:Int):Boolean ={
      //if the member is part of `s` set return it transformed by the function
      if ( s(x) )  s(f(x))
      else false // else it is always false for not being a member of set `s`
    }
    newMap
  }

  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
