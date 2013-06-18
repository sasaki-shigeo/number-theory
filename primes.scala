import scala.math._
import scala.collection.immutable._

object primes extends IndexedSeq[Int] {
  private val buf = new scala.collection.mutable.ArrayBuffer[Int]
  buf ++= Seq(2,3,5,7,11,13,17,19)

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
    if (true && divisors_3_5_7_11_13_17_31(n) != 1) {
      return false
    }
    val limit = sqrt(n).toInt
    // var i = 1   // buf(0) == 2, so the odd primes start from 3 == buf(1)
    var i = 7
    // remember buf take 7 = [2,3,5,7,11,13,17]
    // and the check on these numbers is already done.
    // non-trivial primes number (in the context of this code) starts from
    // 19 == buf(7)
    while (buf(i) <= limit) {
      if (n % buf(i) == 0)
	return false
      i += 1
    }
    true
  }

  override def hasDefiniteSize = false

  def length = Int.MaxValue    // exactly *infinite* if memory possible

  def intExpand(pows: Map[Int, Int]) = {
    for { p <- pows.keys
	  val n = pows(p)
       } yield BigInt(p) pow n
  } product

  def intFactor(positiveInteger: BigInt): Map[Int,Int] = {
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

  private val table8 = Array.tabulate[Int](256){
    case 0   => 0			// error
    case 255 => 255			// 3*5*17
    case k if k % (3*5)  == 0 => 3*5
    case k if k % (3*17) == 0 => 3*17
    case k if k % (5*17) == 0 => 5*17
    case k if k % 3      == 0 => 3
    case k if k % 5      == 0 => 5
    case k if k % 17     == 0 => 17
    case _   => 1
  }

  /* does not check k is divisible by 3 since have done by table8 */
  private val table10 = Array.tabulate[Int](1024){
    case 0   => 0			// error
    case k if k % (11*31) == 0 => 11*31
    case k if k % 11      == 0 => 11
    case k if k % 31      == 0 => 31
    case _   => 1
  }

  /* does not check k is divisible by 3,5 since have done by table8 */
  private val table12 = Array.tabulate[Int](4096){
    case 0   => 0			// error
    case k if k % (7*13)  == 0 => 7*13
    case k if k % 7       == 0 => 7
    case k if k % 13      == 0 => 13
    case _   => 1
  }

  /**
   * In binary arithmetics, we can decide a given number is even or not
   * by checking the last bit is 0 or not.
   *
   * The following equations suggest how to decide a given number can
   * be divided by either 3, 5, 7, 11, 13, 17 or 31.
   *
   * 2^8-1  =  255 = 3 * 5 * 17
   * 2^10-1 = 1023 = 3 * 11 * 31
   * 2^12-1 = 4095 = 3 * 5 * 7 * 13
   *
   * 2^8-1 = 3 * 5 * 17 means byte-wise sum of an integer keeps
   * moduli divided by 3, 5 and 17.
   * For example, the byte-wise sum of 0xACE135 = (0xAB + 0xC1 + 0x35)
   * = 0x1ce = 450 = 2 * 3^2 * 5^2.  So, 0xACE135 is divisible by 3 and 5.
   * Make sure this fact!  0xACE135 = 11329845 = 3 * 5 * 257 * 293.
   *
   * Note: 0xACE135 is divisible by neither 3^2, 5^2 nor 2.
   *
   * So are 10bits-wise sum and 12bits-wise sum.
   */
  final def divisors_3_5_7_11_13_17_31(n: Int): Int = {
    val bytewiseSum =
      ((n & 0xff)
       + ((n >>> 8) & 0xff)
       + ((n >>> 16) & 0xff)
       + ((n >>> 24) & 0x7f)
      ) % 0xff

    val bit10wiseSum =
      ((n & 0x3ff)
       + ((n >>> 10) & 0x3ff)
       + ((n >>> 20) & 0x3ff)
       + ((n >>> 30) & 0x1)
      ) % 0x3ff
    
    val bit12wiseSum =
      ((n & 0xfff)
       + ((n >>> 12) & 0xfff)
       + ((n >>> 24) & 0x7f)
       ) % 0xfff       

    return table8(bytewiseSum) *
           table10(bit10wiseSum) *
           table12(bit12wiseSum)
  }

  def divisors_2_3_5_7_11_13_17_31(n: Int): Int =
    divisors_3_5_7_11_13_17_31(n) * (2 - (n & 1))

  def main(args: Array[String]) {
    for (_ <- 1 to 1000000) {
      val n = (1000000 * math.random).toInt
      val divisors = divisors_2_3_5_7_11_13_17_31(n)
      if ((n % 2 == 0 && divisors % 2 != 0) ||
	  (n % 3 == 0 && divisors % 3 != 0) ||
	  (n % 5 == 0 && divisors % 5 != 0) ||
	  (n % 7 == 0 && divisors % 7 != 0) ||
	  (n % 11 == 0 && divisors % 11 != 0) ||
	  (n % 13 == 0 && divisors % 13 != 0) ||
	  (n % 17 == 0 && divisors % 17 != 0) ||
	  (n % 31 == 0 && divisors % 31 != 0)
	) {
	    printf("divisors miss!  n = %d, table = %d,  factors = %s%n", n, divisors, intFactor(n))
	  }
    }

    println("Test done")

    val start = System.currentTimeMillis
    for (i <- 1000000 to 6000000 by 1000000) {
      println(primes(i))
    }
    val stop  = System.currentTimeMillis
    printf("time: %s ms%n", stop - start)
  }
}
