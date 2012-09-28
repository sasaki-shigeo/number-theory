import scala.math._
import scala.collection.immutable._

object primes extends IndexedSeq[Int] {
  private val buf = new scala.collection.mutable.ArrayBuffer[Int](6000000)
  buf ++= Seq(2,3,5,7)

  def apply(ix: Int): Int = {
    if (ix < buf.size)
      buf(ix)
    else {
      buf.sizeHint(ix + 1)

      var n = buf.last + 2
      while (buf.size - 1 < ix) {
	if (prime_?(n))
	  buf += n
	n += 2
      }	// buf.size - 1 == ix
      buf(ix)
    } 
  }

  private def prime_?(n: Int): Boolean = {
    val limit = sqrt(n).toInt
    var i = 1	   // buf(0) == 2, so the odd primes start from 3 == buf(1)
    while (buf(i) <= limit) {
      if (n % buf(i) == 0)
	return false
      i += 1
    }
    true
  }

  override def hasDefiniteSize = false

  def length = Int.MaxValue    // exactly *infinite* if memory possible


  def intFactor(positiveInteger: Int): Map[Int,Int] = {
    require(positiveInteger > 0)
    var factorMap = IntMap.empty[Int]
    var n = positiveInteger
    val it = this.iterator
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
}


