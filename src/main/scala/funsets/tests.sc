import funsets.FunSets.{Set,  contains,   singletonSet, printSet}

def isEven(x: Int) = x % 2 == 0
def isEvenlyDivisibleByFour(x:Int) = x % 4 == 0
def double(x:Int):Int = x * 2

def intersect(s: Set, t: Set): Set = {
  def bothtrue(x: Int): Boolean = s(x) && t(x)
  bothtrue
}

/**
  * Returns a set transformed by applying `f` to each element of `s`.
  */
def map(s: Set, f: Int => Int): Set = {
  def newMap(x:Int):Boolean ={
    //if the member is part of the original set return it tranformed by the function
    if ( s(x) )  s(f(x))
    else false // else it is always false for not being a member of set `s`
  }
  newMap
}

val bound = 10
/**
  * Returns whether all bounded integers within `s` satisfy `p`.
  */

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    //      println("testing " + a.toString)
    if (a > bound)
      {
        println("testing:" + a.toString + "against ubound" + bound.toString )
        true
      }
//      the problem here occurs when integers in `s` are transformed
//      take for example -9, run through map(
    else if (contains(s,a) && !intersect(s, p)(a)) {

      println("testing:" + a.toString )
      println("contains returns: " + contains(s,a).toString() )
      println("intersect returns" + intersect(s, p)(a).toString )
      false
    }
    else iter(a + 1)
  }
  iter(-bound)
}


val s9 = singletonSet(-9)
isEven(-9)
val
val s = map(isEven,double)
s(-9) // when you apply double against any value in is_even
val e:Set = isEven
printSet(s)
contains(s, -9)
contains(isEven,-9)
intersect(s, isEven)(-9)


forall(map(isEven,double),isEven)