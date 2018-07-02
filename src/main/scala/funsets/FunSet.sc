type Set = Int => Boolean
import funsets.FunSets.{Set, bound, intersect, _}


def union(s: Set, t: Set): Set = {
  def eitherTrue(x: Int):Boolean = {
    s(x) || t(x)
  }
  eitherTrue
}

def isPositive : Set = {
  def returnval(x:Int):Boolean = x > -5000
    returnval
}

def isSpecificNegative : Set = {
  def returnval(x:Int):Boolean = x != -50000
  returnval
}

val bound2 = 1000

def forall2(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
          println("testing " + a.toString)
    if (a > bound2) {
      true
    }

    else if (!intersect(s,p)(a) == true) {
      println(s(a).toString)
      println(p(a).toString)
      false
    }
    else iter(a+1)
  }
  iter(-bound2)
}


forall2(isPositive,isSpecificNegative)

!intersect(isPositive,isPositive)(-1000)


