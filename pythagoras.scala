def pythagoras_numbers(limit: Int) =
  for {
    c <- 3 to limit
    a <- 1 until c
    b <- a+1 until c
    if a*a + b*b == c*c
    if gcd(a,b) == 1
  } yield (a,b,c)
