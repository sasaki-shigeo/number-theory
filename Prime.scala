object Prime extends Seq[Int] {
  private val buf = new scala.collection.mutable.ArrayBuffer[Int]()
  buf ++= Seq(2,3,5,7)

  def apply(ix: Int): Int = {
    if (ix < buf.size)
      buf(ix)
    else {
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
    val limit = math.sqrt(n).toInt
    var i = 1	   // buf(0) == 2, so the odd primes start by buf(1)
    while (buf(i) <= limit) {
      if (n % buf(i) == 0)
	return false
      i += 1
    }
    true
  }

  def length = -1

  def iterator = new Iterator[Int] {
    private var ix = 0;
    def hasNext = true
    def next = {
      val a = apply(ix)
      ix += 1
      a
    }
  }


  val primesTo100 = Seq(2,3,5,7) ++ {
    for {
      n <- 11 to 100 by 2
      if Seq(2,3,5,7) forall { n % _ != 0 }
    } yield n
  }

  val primesTo1000 = Vector(2,3,5,7,11,13,17,19,23,29,31) ++ {
    for {
      n <- 33 to 1000 by 2
      if Vector(2,3,5,7,11,13,17,19,23,29,31) forall { n % _ != 0 }
    } yield n
  }

  val primesTo10000 = primesTo100 ++ {
    for {
      n <- 101 to 10000 by 2
      if primesTo100 forall { n % _ != 0 }
    } yield n
  }

  lazy val primesTo1000000 = primesTo1000 ++ {
    for {
      n <- 1001 to 1000000 by 2
      if primesTo1000 forall { n % _ != 0 }
    } yield n
  }

  val oddPrimes: Stream[Int] = Stream.cons(3, {
    for {
      n <- Stream.from(5, 2)	// 5, 7, ...
      val limit = math.sqrt(n)
      val primes = oddPrimes.takeWhile(_ <= math.sqrt(n)).toIndexedSeq
      if primes forall { n % _ != 0 }
    } yield n
  })

  val primes = Stream.cons(2, oddPrimes)
}

