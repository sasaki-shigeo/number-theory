def pythagorean_numbers(limit: Int) =
  for {
    c <- 3 to limit
    a <- 1 until c
    b <- a+1 until c
    if a*a + b*b == c*c
  } yield (a,b,c)


import scala.math._
    
def primitive_pythagorean_numbers(limit: Int) =
  for {
    m <- 2 to sqrt(limit).toInt by 2
    n <- 1 until sqrt(limit).toInt by 2
    m*m + n*n < limit
  } yield ((m*m - n*n).abs, 2*m*n, m*m + n*n)
