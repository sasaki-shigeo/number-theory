import scala.collection.immutable._

def intFactor(positiveInteger: Int): Map[Int,Int] = {
  require(positiveInteger > 0)
  var factorMap = IntMap.empty[Int]
  var n = positiveInteger
  val it = primes.iterator
  var p = it.next
  while (n >= p) {
    if (n % p == 0) {
      factorMap += p -> (factorMap.getOrElse(p, 0) + 1)
      n /= p
    }
    else {
      p = it.next
    }
  }
  return factorMap.withDefaultValue(0)
}

// naive rad
def radical(n: Int): Int = intFactor(n).keySet.product

def radicalSet(positiveInteger: Int): scala.collection.Set[Int] = {
  var n = positiveInteger
  var s = new scala.collection.mutable.LinkedHashSet[Int]
  val it = primes.iterator
  var p = it.next			// a prime number
  while (n >= p) {
    if (n % p == 0) {
      s += p
      do {
	n /= p
      } while (n % p == 0)
    }
    p = it.next				// the next prime number
  }
  return s
}

def rad(a: Int, b: Int, c: Int): Int =
  (radicalSet(a) | radicalSet(b) | radicalSet(c)).fold(1){_*_}

def phi(n: Int): Int =
  intFactor(n).foldLeft(n) { (acc,m) => acc / m._1 * (m._1 - 1) }

def gcd(m: Int, n: Int): Int = {
  val r = m % n
  if (r == 0)
    n
  else
    gcd(n, r)
}

def totient(n: Int): Int =
  1 until n count { gcd(n, _) == 1}


// def gcd3(k:Int, m:Int, n:Int):Int = gcd(gcd(k, m), n)
