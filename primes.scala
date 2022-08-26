//
// Prime Numbers
//

val primes_10 = Vector(2, 3, 5, 7)
val primes_31 = Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)
val primes_100 = Vector(2, 3, 5, 7) ++ (
  for {
    p <- 11 until 100 by 2
    if p % 3 != 0
    if p % 5 != 0
    if p % 7 != 0
  } yield p
)

def isqrt(n:Long): Int = {
  assert(n >= 0)

  var x0 = (n + 1) / 2
  var x1 = (x0 + n / x0) / 2
  while (x1 < x0) {
    x0 = x1
    x1 = (x0 + n / x0) / 2
  }
  x0.toInt
}

import scala.annotation.tailrec

import scala.collection.SeqOps
import scala.collection.AbstractSeq
import scala.collection.View

val primes_1000 = primes_31 ++ (
  for { p <- 37 until 1000 by 2
        if primes_31.drop(1).forall(p % _ != 0)
  } yield p
)

lazy val primes_10000 = primes_100 ++ (
  for { p <- 101 until 10000 by 2
        if primes_100.drop(1).forall(p % _ != 0)
  } yield p
)

lazy val primes_1000000 = primes_31 ++ (
  for { p <- 1001 until 1000000 by 2
        if primes_1000.view.drop(1).forall(p % _ != 0)
  } yield p
)

def prime_?(n:Int):Boolean = {
  (primes_10000 contains n) || primes_10000.forall(n % _ != 0)
}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import scala.collection.mutable.BitSet
import scala.collection.mutable.LinkedHashMap
import scala.collection.IndexedSeq


trait Eratosthenes extends IndexedSeq[Boolean] {
  override def toString() = {
    val buf = new StringBuilder(this.length + this.length / 8)

    for (k <- 0 until this.length) {
      buf += (if (this(k)) '1' else '0')
      if (k % 8 == 7) buf += ' '
    }
    buf.result()
  }

  def extendTo(n:Int):Unit

  def prime_?(n:Long) = {
    if (n <= 1) {
      false
    }
    else if (n < this.length) {
      this(n.toInt)
    }
    else {
      (3 to math.sqrt(n.toDouble).toInt by 2).filter(this(_)).forall(n % _ != 0)
    }
  }

  class OddPrimeIterator(outer:Eratosthenes, start:Int = 3) extends Iterator[Int] {
    private var ix = start
    private var notEmpty = true
    
    def hasNext = notEmpty

    def next() = {
      val result = ix
      do {
        ix += 2
        notEmpty &= (ix < outer.length)
      } while (notEmpty && ! outer(ix))

      result
    }
  }

  def primes:Iterator[Int] = Iterator(2) ++ (new OddPrimeIterator(this))
}

object eratosthenes extends Eratosthenes {
  private var sz = 32

  val bitmap = new ArrayBuffer[Int](sz/32 + 1)
  clear()

  def clear():Unit = {
    bitmap.clear()
    // ix:   0 1 2 3|4 5 6 7|8 9 10 11|12 13 14 15|
    // bit:  1 2 4 8|1 2 4 8|1 2  4  8| 1  2  4  8|
    // p:        4 8|  2   8|        8|    2      |
    // hex:        C|      A|        8|          2|
    //
    // p:    17 19| 23|  | 29 31|
    // bit:   2  8|  8| 0|  2  8|
    // hex:      A|  8| 0|     A|
    //
    // As a result, the first content is 0xA08A_28AC
    //
    bitmap += 0xA08A_28AC
    sz = 32
  }

  def length = sz

  def apply(n:Int):Boolean = {
    val high = n / 32
    val low  = n % 32
    
    (bitmap(high) & (1 << low)) != 0
  }

  def update(n:Int, b:Boolean) = {
    val high = n / 32
    val low  = n % 32

    if (b) {
      bitmap(high) |= (1 << low)
    }
    else {
      bitmap(high) &= ~(1 << low)
    }
  }

  def extendTo(n:Int) = {
    // The initial values of bitmap(2..end) are following:
    // ix:  0 1 2 3|4 5 6 7|
    // odd:   1   3|  5   7|
    // bit:   2   8|  2   8|
    // hex:       A|      A|
    bitmap ++= (for (i <- bitmap.length until n/32 + 1) yield 0xAAAA_AAAA)
    
    for (p <- 3 to math.sqrt(n).toInt by 2) {
      if (this(p)) {
        for (n <- p*p until n by 2*p) {
          this(n) = false
        }
      }
    }

    sz = n
  }
}

object eratosthenes1 extends Eratosthenes {
  val bitset = BitSet(2, 3, 5, 7)

  def clear() = {
    bitset.clear()
    bitset += 2
  }
  def length = bitset.max + 2
  def apply(p:Int) = bitset.apply(p)
  def update(p:Int, bool:Boolean) = bitset.update(p, bool)
  def extendTo(n:Int) = {
    val start = bitset.max + 2
    bitset ++= start to n by 2
    for (p <- 3 to math.sqrt(n).toInt by 2; if this(p)) {
      // nextComposite calculation:
      // if start is divisible by p, return the start,
      // otherwise return the next odd divisible by p.
      // It is ceil[(start - p) / 2p] * 2p + p.
      // Unfortunately, the behavior of JVM int division is simular to truncate,
      // not ceil(_ / _).
      // Instead of ceil(_ / _), we adopt floor((_ + .999...) / _).
      // For example, let p = 5, start = 25, 27, ..., 25
      // start == 25 => 25
      // start == 27 => 35
      // start == 29 => 35
      // ...
      // start == 35 => 35
      // 
      // start  +(p-1)  /(2p)  *(2p)   +p
      // ----------------------------------
      // 25     29      2      20      25
      // 27     31      3      30      35
      // ...
      // 35     39      3      30      35 
      //
      val nextComposite = ((start + p - 1) / (2 * p)) * (2 * p) + p
      for (k <- nextComposite to n by 2 * p) {
        bitset -= k
      }
    }
  }
}

object eratosthenes2 extends Eratosthenes {
  private var sz = 0

  // ix: 0 1 2 3 ... 31|32 33 ... 63|
  //  n: 3 5 7 9 ... 65|67 ..... 129| 
  val bitmap = new ArrayBuffer[Int]()
  clear()

  def length = sz

  def apply(n:Int):Boolean = {
    if (n < 0) {
      throw new IndexOutOfBoundsException(n)
    }
    else if (n == 0 || n == 1) {
      false
    }
    else if (n == 2) {
      true
    }
    else {
      val high = (n - 3) / 64
      val low  = (n - 3) % 64 / 2
    
      (bitmap(high) & (1 << low)) != 0
    }
  }

  def update(n:Int, b:Boolean) = {
    if (n == 0 || n == 1 || n % 2 == 0) {
      throw new ArrayIndexOutOfBoundsException(n)
    }

    val high = (n - 3) / 64
    val low  = (n - 3) % 64 / 2

    if (b) {
      bitmap(high) |= (1 << low)
    }
    else {
      bitmap(high) &= ~(1 << low)
    }
  }

  def clear():Unit = {
    sz = 9
    bitmap.clear()
    bitmap += 0xffff_ffff
  }

  def extendTo(n:Int) = {
    val old_sz = sz
    sz = n
    bitmap.sizeHint((n-3)/64)
    for (i <- bitmap.length until 1 + (n - 3)/64) {
      bitmap += 0xffff_ffff
    }
    
    for (p <- 3 to math.sqrt(n).toInt by 2) {
      if (this(p)) {
        for (n <- p*p until n by 2*p) {
          this(n) = false
        }
      }
    }
  }

  // def primes = 2 +: (for { p<-3 until sz by 2; if this(p) } yield p )
}


//
// There are bugs.
//
object eratosthenes6 extends Eratosthenes {
  private var sz = 0

  // Indexes given by apply or update are 6n ± 1,
  // for example,  5, 7,  11, 13,  17, 19,  23, 25,  29, 31,  35, 37,  ...
  // ix: 0  1  2  3  4  5 ... 30 31| 32  33 ...  62  63|
  //  n: 5  7 11 13 17 19 ... 95 97|101 103 ... 191 193|
  //
  //  n = 3 * ix + 5 if ix is even
  //  n = 3 * ix + 4 if ix is odd
  //
  //  ix = (n - 4) / 3 if n % 6 == 1
  //  ix = (n - 5) / 3 if n % 6 == 5 != 1
  //  ix = (n - 4) / 3 everytime
  //
  

  val bitmap = new ArrayBuffer[Int]((sz - 3) / 64 + 1)

  clear()
  
  def length = sz

  def apply(n:Int):Boolean = {
    if (n == 3) {
      true
    }
    else if (n == 0 || n == 1 || n % 2 == 0 || n % 3 == 0) {
      false
    }
    else {
      val ix   = (n - 4) / 3
      val high = ix / 32
      val low  = ix % 32

      (bitmap(high) & (1 << low)) != 0
    }
  }

  def update(n:Int, b:Boolean) = {
    if (n == 0 || n == 1 || n % 2 == 0 || n % 3 == 0) {
      // do nothing
    }
    else {
      val ix   = (n - 4) / 3
      val high = ix / 32
      val low  = ix % 32

      if (b) {
        bitmap(high) |= (1 << low)
      }
      else {
        bitmap(high) &= ~(1 << low)
      }
    }
  }
  
  def clear():Unit = {
    sz = 25
    bitmap.clear()
    bitmap += 0xffff_ffff
  }

  def extendTo(n:Int) = {
    val old_sz = sz
    sz = n

    val ix = (n - 4) / 3
    bitmap.sizeHint(ix / 32 + 1)
    for (i <- bitmap.length until 1 +  ix / 32) {
      bitmap += 0xffff_ffff
    }
    
    for (p <- 5 to math.sqrt(n).toInt by 2) {
      if (this(p)) {
        for (n <- p*p until n by 2*p) {
          this(n) = false
        }
      }
    }
  }

  // def primes = 2 +: 3+: (for { p<-5 until sz by 2; if this(p) } yield p )
  override def primes:Iterator[Int] = Iterator(2, 3) ++ (new OddPrimeIterator(this, 5))
}

/**
 * Returns integer factors in multiset (Map of Int -> Int) style
 *
 * Algorithm: division by the eratosthenes sieved primes.
 *
 * @param: An integer to be factored
 * @returns: The multiset of primes represented by Map[Int, Int]
 * Keys of the multiset: prime numbers
 * Values of the multiset: degrees of the prime numbers
 *
 * For example, intFactor(14000) => Map(2->4, 5->3, 7->1) which means
 * 2^4 * 5^3 * 7 = 7 * 2 * 2^3 * 5^3 = 7 * 2 * 10^3 = 14000
 *
 */
def intFactor(m:Int):Map[Int, Int] = {
  eratosthenes2.extendTo(1 + math.sqrt(m).toInt)

  val table = LinkedHashMap[Int, Int]().withDefaultValue(0)
  var n  = m
  val it = eratosthenes2.primes
  var p = it.next()       // p == 2
  while (n > 1) {
    while (n % p == 0) {
      table(p) += 1
      n /= p
    }
    if (it.hasNext) {
      p = it.next()
    }
    else {   //  there are no prime candicates in TABLE.
      table(n) = 1
      return table.toMap
    }
  }
  table.toMap
}

def intFactor(m:Int):Iterator[(Int,Int)] = {
  eratosthenes2.extendTo(1 + math.sqrt(m).toInt)

  new Iterator[(Int, Int)] {
    val primes = eratosthenes2.primes
    var n = if (m <= 1) 1 else m

    def hasNext = (n != 1)
    def next() = {
      @tailrec def loop(p:Int):(Int,Int) = {
        if (n % p == 0) {
          var d = 0
          do {
            n /= p
            d += 1
          } while (n % p == 0)

          p->d  // result
        }
        else if (primes.hasNext) {
          loop(primes.next())
        }
        else {
          val residual = n
          n = 1

          residual -> 1   // the last result
        }
      }

      loop(primes.next())    // the initial value of primes.next() is 2.
    }
  }
}


def pollardRho(n:BigInt, x0:BigInt = 2:BigInt) = {
  def f(x:BigInt) = (x * x + 1) % n

  @tailrec def loop(x:BigInt, y:BigInt):Option[BigInt] = {
    val g = (x - y) gcd n
    if (g == n) {
      None
    }
    else if (g == 1) {
      loop(f(x), f(f(y)))
    }
    else {
      Some(g)
    }
  }

  loop(f(n), f(f(n)))
}

def IntFactor(n:BigInt) = new Iterator[BigInt] {
  var m = n

  def hasNext = (m != 1)

  def next() = {
    pollardRho(m) match {
      case Some(d) => {
        m /= d
        d
      }
      case None    => {
        val residual = m
        m = 1
        residual
      }
    }
  }
}
