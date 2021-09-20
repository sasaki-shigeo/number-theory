/*
 * (package) object intmath
 *
 */

import scala.annotation.tailrec

object intmath {
    @inline def abs[T](n:T)(implicit num:Numeric[T]) = num.abs(n)
    def max[T](x:T, y:T)(implicit ord:Ordering[T]) = ord.max(x, y)
    def min[T](x:T, y:T)(implicit ord:Ordering[T]) = ord.min(x, y)
    def gcd[T](m:T, n:T)(implicit integ:Integral[T]):T = {
        @tailrec def gcd_(n:T, r:T):T = {
            if (integ.equiv(r, integ.zero)) {
                n
            }
            else {
                gcd_(r, integ.rem(n, r))
            }
        }

        if (integ.equiv(m, n)) {
            n
        }
        else if (integ.gteq(abs(m), abs(n))) {
            gcd_(abs(m), abs(n))
        }
        else {
            gcd_(abs(n), abs(m))
        }
    }

    def lcm[T](m:T, n:T)(implicit integ:Integral[T]):T = {
        integ.quot(integ.times(m,
                               gcd(m, n)),
                   n)
    }

    def isqrt[T](n:T)(implicit integ:Integral[T]):T = {
        val zero = integ.zero
        val one  = integ.one
        val two  = integ.plus(one, one)
        @inline def plus(m:T, n:T) = integ.plus(m, n)
        @inline def quot(m:T, n:T) = integ.quot(m, n)
        @inline def lteq(m:T, n:T) = integ.lteq(m, n)
        @inline def iszero(n:T) = integ.equiv(n, zero)

        @tailrec def newton(x0:T):T = {
            val x1 = quot(plus(x0, quot(n, x0)),
                          two)
            if (lteq(x0, x1)) {
                x0
            }
            else {
                newton(x1)
            }
        }

        if (iszero(n)) {
            n
        }
        else if (lteq(zero, n)) {
            newton(quot(plus(n, one),
                        two))
        }
        else {
            throw new IllegalArgumentException("negative integer: %d".format(n))
        }
    }

    def icbrt[T](n:T)(implicit integ:Integral[T]):T = {
        val zero = integ.zero
        val one  = integ.one
        val two  = integ.plus(one, one)
        val three = integ.plus(two, one)

        @inline def plus(m:T, n:T) = integ.plus(m, n)
        @inline def times(m:T, n:T) = integ.times(m, n)
        @inline def quot(m:T, n:T) = integ.quot(m, n)
        @inline def lteq(m:T, n:T) = integ.lteq(m, n)
        @inline def iszero(n:T) = integ.equiv(n, zero)

        @tailrec def newton(x0:T):T = {
            val x1 = quot(plus(times(two, x0),
                               quot(n, times(x0, x0))),
                          three)
            if (lteq(x0, x1)) {
                x0
            }
            else {
                newton(x1)
            }
        }

        if (iszero(n)) {
            n
        }
        else if (lteq(zero, n)) {
            newton(quot(plus(n, two),
                        three))
        }
        else {
            throw new IllegalArgumentException("negative integer: %d".format(n))
        }
    }

    def getExponent(n:BigInt):Int = n.bitLength - 1
    def getExponent(n:Int):Int = {
        @tailrec def loop(n:Int, len:Int): Int = {
            if (n == 0) {
                len
            }
            else {
                loop(n >> 1, len + 1)
            }
        }

        if (n == Int.MinValue) {
            31
        }
        else {
            loop(abs(n), 0) - 1
        }
    }

    def getExponent(n:Long):Int = {
        @tailrec def loop(n:Long, len:Int): Int = {
            if (n == 0) {
                len
            }
            else {
                loop(n >> 1, len + 1)
            }
        }

        if (n == Long.MinValue) {
            63
        }
        else {
            loop(abs(n), 0) - 1
        }
    }
}

object intmathTest extends App {
    println("foo")

    assert(intmath.gcd(99, 100) == 1)
    assert(intmath.gcd(70, 100) == 10)
    assert(intmath.gcd(1, 100) == 1)
    assert(intmath.gcd(0, 100) == 100)
    assert(intmath.gcd(0, 0) == 0)
    assert(intmath.gcd(123456789, 987654321) == 9)
    assert(intmath.gcd(123456789L, 987654321L) == 9)
    assert(intmath.gcd(1234567890123456789L, 987654321987654321L) == 9)

    assert(intmath.isqrt(0) == 0)
    assert(intmath.isqrt(1) == 1)
    assert(intmath.isqrt(100) == 10)
    assert(intmath.isqrt(10000) == 100)
    assert(intmath.isqrt(100000000L) == 10000)
    assert(intmath.isqrt(1000000000000L) == 1000000)

    assert(intmath.icbrt(0) == 0)
    assert(intmath.icbrt(1) == 1)
    assert(intmath.icbrt(1000) == 10)
    assert(intmath.icbrt(1000000) == 100)
    assert(intmath.icbrt(1000000000L) == 1000)

    for (i <- 1 to 17) {
        assert(intmath.getExponent(i) == math.getExponent(i.toFloat))
    }
    for (i <- -1 to -17 by -1) {
        assert(intmath.getExponent(i) == math.getExponent(i.toFloat))
    }

    for (i <- 1 to 31) {
        assert(intmath.getExponent(1 << i) == math.getExponent((1 << i).toDouble))
    }
    for (i <- 32 to 63) {
        assert(intmath.getExponent(1L << i) == math.getExponent((1L << i).toDouble))
    }

    assert(intmath.getExponent(Int.MinValue) == math.getExponent(Int.MinValue.toDouble))
    assert(intmath.getExponent(Long.MinValue) == math.getExponent(Long.MinValue.toDouble))
}

intmathTest.main(Array[String]())