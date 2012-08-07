// Mersenne number
//   M[n] = 2^n - 1
def mersenne(n: Int) = BigInt(2).pow(n) - 1

// Note: Mersenne numnber M[n] = 2^n - 1 is a prime number only if n is.
// i.e. if n is not prime => so is not M[n].
//
// proof) Let n = a*b, this is, n is not prime,
//   M[n] = 2^(a*b) - 1 = (2^a)^b - 1 = (2^a - 1)*((2^a)^(b-1) + ... +1)
// is not prime.

//
// Lucas Test
//
// Define a sequence S[n] as below.
//   S[0] := 4
//   S[n+1] := S[n]^2 - 2
//
// Lucas and Lehmer proofed that
//   M[n] is a prime number iff S[n-2] = 0 (mod M[n])
// The above examination is called Lucas test.
//
// For example,
//   M[3] = 2^3 - 1 = 7
//   S[3-2] = S[1] = 14 = 0 (mod 7)
//   M[5] = 2^5 - 1 = 31
//   S[5-2] = S[3] = 37634 = 1214 * 31 = 0 (mod 31)
//   M[7] = 2^7 - 1 = 127
//   S[7-2] = S[5] = 2005956546822746114 = 15794933439549182 * 127 = 0 (mod 127)
//   M[11] = 2^11 - 1 = 2047 = 23 * 89
//   S[11-2] = S[9] = 111 (mod 2047)
//
// Haskell can calculate the infinete S[n] as below.
//   [S[0], S[1], ...] = map S [1..]
//                     = [S[n] | n <- [1..]]
// But below is faster because of no redudant calculation.
//      iterate (λs->s^2 - 2) 4
//      scanl (λs _->s^2 - 2) 4 [1..]
//
// Scala has no (infinete) iterate function. But it has scalLeft.
// (1 to 5).scanLeft(BigInt(4)){(s,_) => s*s-2}
// == Vector(4, 14, 194, 37634, 1416317954, 2005956546822746114)
//
// It means S[n] = (1 to n).foldLeft(4:BigInt){(s,_) => s*s - 2}
//
// Now is the step to refine the Lucas test algorithm.
// People who try calculating a large S[n] by the above algorithm
// are going to see it is too slow.
// Do you aware that it is not necessary to calculate a large large S[n]?
//
// The Lucas test S[n-2] = 0 (mod M[n]) requires modular arithmetic.
// The below S'[n] does not grow too largely if calculating modulo each time.
//   S'[1] = S'[0]^2 + 2 (mod M[n])
//   S'[2] = S'[1]^2 + 2 (mod M[n])
// and so on.
//
// 　S'[n] = (1 to n-2).foldLeft(4:BigInt){(s,_) => (s*s - 2) % M[n]}

def isMersennePrime(n: Int) = {
  val p = mersenne(n)
  (3 to n).foldLeft(4:BigInt){(s,_) => (s*s-2) % p} == 0
}


