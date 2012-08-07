object PrimesTest {
  //
  // Fermat Test
  //   if a^n != a (mod n) => n is not prime.
  // 
  //   if a^(p-1) == b^(p-1) == 1 (mod p) => p is probably prime.
  // Typically a = 2, b = 3
  //
  // ---------------------
  // |Theoretical validity
  //
  // Fermat's little theorem:
  //   if p is prime => a^(p-1) = 1 (mod p)
  // is always true.  But its converse
  //   a^(p-1) = 1 (mod p) => p is prime
  // is false for some (but rare) a.
  //
  // Most of non-prime numbers do not satisfy a^(n-1) = 1 (mod n),
  // and a few a_1, a_2, ... can decide whether n is prime or not
  // by Fermat test except for the Carmichael numbers.
  // The Carmichael numbers are called pseudo prime since a Carmichael
  // number n satisfies Fermat test but n is not prime.
  //
  def fermatTest(p: BigInt)(implicit a: BigInt = 3) =
    a.modPow(p-1, p) == BigInt(1)

}
