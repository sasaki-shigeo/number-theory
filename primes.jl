#
# To load this file, type following
#
# ```sh
# include("primes.jl")
# ```
#

primesTo10 = [2, 3, 5, 7]

function isOddPrimeBy(n::Integer; by=[3,5,7])::Bool
    @assert n % 2 != 0 "non even assumed: $n"
    all(by) do p
        n % p != 0
    end
end

primes5bits = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]

function oddSieve(odds; by = primes5bits)
    [ n  for n in odds
         if all(by) do p
             n % p != 0
         end ]
end

#
# Note: Let u and v be vectors,
# [ u; v ] means vcat(u, v)
# 

primesTo100 = [ primesTo10; oddSieve(11:2:99, by = [3, 5, 7]) ]

primesTo1000 = [ primes5bits;
                 oddSieve(37:2:999, by = @view primes5bits[begin+1:end]) ]

primesTo10000 = [ primesTo100;
                  oddSieve(101:2:9999, by = @view primesTo100[begin+1:end]) ]

# primesTo1000000 = [ primesTo1000;
#                     oddSieve(1001:2:999999, by = @view primesTo1000[begin+1:end]) ]


function isPrime(n::Integer; by = primesTo10000)
    if (n <= last(by))
        n in by
    else
        all(by) do p
            n % p != 0
        end
    end
end

using Base.Iterators

#
# Lists of primes numbers
#
# The constructor creates a list of primes numbers
# in BitArray (bit by bit representation if the index is prime)
# by Eratosthenes sieve algorithm.
#
# The most time-consuming phase of the algorithm is checking if
# N is divisible by 2 (and 3).  It is a nice idea to buid 
# the primes candidate list without evens > 2, initially.
# Fortunately, its bit pattern is quite simple. See below.
#
# BitArray().chunks is the inner representation of BitArray()
#
# oddsAndTwo =  { 2, 3, 5, 7, 9, 11, 13, ... }
# Its bitmap is
#
# | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |...|63 |64 ||65 |66 |...|128|
# | 0 | 1 | 1 | 0 | 1 | 0 | 1 | 0 | 1 |...| 1 | 0 || 1 | 0 |...|  0|
#
# Its reverse ordered:
#
# |128|127|...|66|65||64|63|...| 8| 7| 6| 5| 4| 3| 2| 1|
# | 0 | 1 |...| 0| 1|| 0| 1|...| 0| 1| 0| 1| 0| 1| 1| 0|
#
# Its hexadecimal reporesentation:
# [0x55...55, 0x55..56]
#

struct PrimesBitmap
    bitmap:: BitVector

    function PrimesBitmap(size)
        this = new(falses(size))
        fill!(this.bitmap.chunks,
              0x5555_5555_5555_5555_5555_5555_5555_5555 % typeof(this.bitmap.chunks[1]))
        this.bitmap.chunks[begin] =
            0x5555_5555_5555_5555_5555_5555_5555_5556 % typeof(this.bitmap.chunks[1])

        for p in 3:2:isqrt(size)
            if this.bitmap[p]   # when P is a prime number.
                # p^2, p^2 + p, p^2 + 2p, ... are not prime.
                # Note: p^2 + p, p^2 + 3p, ... are not need to sieve
                # since even are p^2 + p = p(p + 1) and so on.
                # Sieve only p^2, p^2 + 2p, p^2 + 4p, ...
                for q in p^2:2p:lastindex(this.bitmap)
                    this.bitmap[q] = false
                end
            end
        end
        return this
    end
end

function isPrime(n::Integer, by::PrimesBitmap)
    if n <= lastindex(by)
        by.bitmap[n]
    else
        all(by) do p
            n % p != 0
        end
    end
end

Base.collect(primes::PrimesBitmap) = filter(p->primes.bitmap[p],
                                            eachindex(primes.bitmap))

Base.count(primes::PrimesBitmap) = count(primes.bitmap)

#= primesBitmap and is sieve() are obsolete
if ! @isdefined primesBitmap
    primesBitmap = trues(0xffffff)
end

function sieve()
    primesBitmap[1] = false
    print(2, " ")
    for ix in 4:2:length(primesBitmap)
        primesBitmap[ix] = false
    end
    p = 3
    while p <= isqrt(length(primesBitmap))
        if primesBitmap[p]
            print(p, " ")
            for q in p*p:2p:length(primesBitmap)
                primesBitmap[q] = false
            end
        end
        p += 2
    end
end

#sieve()

=#

struct OddBitmap <: AbstractVector{Bool}
    bitmap::BitVector

    function OddBitmap(n::Int)::OddBitmap
        bitmap = trues(n)
    end
end

#
# | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | † the index of the internal bitmap
# | 3 | 5 | 7 | 9 | 11| 13| 15| 17| 19| ‡ the index of PrimesBitmap2
# | 1 | 1 | 1 | 0 | 1 | 1 | 0 | 1 | 1 |   whether is a prime number
#
struct PrimesBitmap2
    bitmap::BitVector

    function PrimesBitmap2(limit::Int)
        this = new(trues((limit - 1) ÷ 2))

        this
    end
end

#
# outside  inside
#       3       1
#       5       2
#       7       3
#     ...
#
# outside == 2 * inside + 1
# inside  == (outside - 1) ÷ 2
#
function Base.getindex(xs::PrimesBitmap2, k::Int)
    if k == 1
        false
    elseif k == 2
        true
    else
        @assert (k & one(k)) == one(k) && k >= 3
        xs.bitmap[(k-1) ÷ 2]
    end
end

function Base.setindex!(xs::PrimesBitmap2, val, k::Int)
    @assert (k & one(k)) == one(k) && k >= 3
    xs.bitmap[(k-1) ÷ 2] = val
end

Base.firstindex(xs::PrimesBitmap2) = 3
Base.lastindex(xs::PrimesBitmap2) = 2 * lastindex(xs.bitmap) + 1
Base.keys(xs::PrimesBitmap2) = firstindex(xs):2:lastindex(xs)
Base.count(xs::PrimesBitmap2) = count(k -> xs.bitmap[k], keys(xs.bitmap))


function show(io::IO, ::MIME"text/plain", xs::PrimesBitmap2)
    summary(io, xs)
    print(io, xs.bitmap)
end

function sieve(xs::PrimesBitmap2)
    for p in 3:2:isqrt(lastindex(xs))
        if xs[p]
            for q in p^2:2p:lastindex(xs)
                xs[q] = false
            end
        end
    end
end

#
# oddPrimesBitmap[1] means 3 is prime.
# oddPrimesBitmap[2] means 5 is prime.
# oddPrimesBitmap[n] means 2n + 1 is prime.
#
if ! @isdefined oddPrimesBitmap
    oddPrimesBitmap = trues(0xfffffff)
end
#
# Let n be a prime candidate index.
# if OddPrimesBitmap[n] then 2n + 1 is prime.
# This function sieves (2n+1)^2, (2n+1)^2 + 2(2n+1), (2n+1)^2 + 2(2n+3) and so on.
# The index of (2n+1)^2 is half of (2n+1)^2 - 1 = (4n^2 + 4n)/2 = 2n^2 + 2n = 2(n+1)n.
# That of (2n+1)^2 + 2(2n+1) is 2(n+1)n + (2n+1)
# That of (2n+1)^2 + 4(2n+1) is 2(n+1)n + 2(2n+1)
#
function oddSieve()
    for n = 1:div(isqrt(2 * length(oddPrimesBitmap) + 1)-1, 2)
        if oddPrimesBitmap[n]
            print(2n+1, " ")
            for drop = 2(n+1)n:(2n+1):length(oddPrimesBitmap)
                oddPrimesBitmap[drop] = false
            end
        end
    end
end

function isPrimeBy(n::Integer; by::BitVector)
    limit = (isqrt(n) - 1) ÷ 2
    if limit > length(oddPrimesBitmap)
        error("too big to vefiry: $n")
    end

    all(2k + 1 for k in 2:limit if oddPrimesBitmap[k]) do p
        n % p != 0
    end
end

#
# Integer Factoring
# (Array Version)
#
function intFactor(n::Integer)
    if n == 0
        throw(ArgumentError("intFactor expects non-zero integer"))
    end

    factors = Vector{Integer}()
    if (n < 0)
        push!(factors, -1)
        n *= -1
    end
    @assert n > 0
    
    while n % 2 == 0
        push!(factors, 2)
        n ÷= 2
    end
    @assert n % 2 != 0

    for ix in eachindex(oddPrimesBitmap)
        if oddPrimesBitmap[ix]
            p = 2 * ix + 1
            # print(p, " ")
            while n % p == 0
                push!(factors, p)
                n ÷= p
                if n == 1
                    return factors
                end
            end
        end
    end
    push!(factors, n)
    return factors
end

#
# Integer Factoring
# (Dict Version)
#
function intFactor(n::Integer)
    if n == 0
        throw(ArgumentError("intFactor expects non-zero integer but 0"))
    end

    factors = Dict{Integer, Int}()
    if (n < 0)
        factors[-1] = 1
        n *= -1
    end
    @assert n > 0
    
    d = 0
    while n % 2 == 0
        d += 1
        n ÷= 2
    end
    if d != 0
        factors[2] = d
    end

    if n == 1
        return factors
    end

    @assert n % 2 != 0

    for ix in eachindex(oddPrimesBitmap)
        if oddPrimesBitmap[ix]
            p = 2 * ix + 1
            # print(p, " ")
            d = 0
            while n % p == 0
                d += 1
                n ÷= p
                if n == 1
                    factors[p] = d
                    return factors
                end
            end
            if d != 0
                factors[p] = d
            end
        end
    end
    factors[n] = 1
    return factors
end

divisors(n::Integer) = [ k for k in 1:n-1 if n % k == 0 ]

coprimes(n::Integer) = [ k for k in 1:n-1 if gcd(n, k) == 1 ]

isPerfect(n::Integer) = sum(divisors(n)) == n

perfects(limit::Integer) = [ n for n in 2:limit if isPerfect(limit) ]

isAmical(m::Integer, n::Integer) = divisors(m) == n && divisors(n) == m

amicals(limit::Integer) = [ (m, n) for n in 3:limit for m in 2:n-1 if isAmical(m, n) ]

mersenne(n::Int) = BigInt(2)^n - 1

function mersennePrimesByEratosthenes()
    for n in primesTo100
        m = mersenne(n)
        if isPrimeBy(m, by=oddPrimesBitmap)
            println("$n: $(m)")
        end
    end
end

function perfectNumbersByEratosthenes()
    for n in primesTo100
        m = mersenne(n)
        if isPrimeBy(m, by=oddPrimesBitmap)
            println("$n: $(m<<(n-1))")
        end
    end
end

isPrimeByFermat2(n::Integer) = powermod(2, n-1, n) == 1
isPrimeByFermat3(n::Integer) = powermod(3, n-1, n) == 1

#
# Note:
#   isPrimeByFermat2(mersenne(11)) is true but mersenne(11) is not prime.
#   isPrimeByFermat2(mersenne(23)) is also true but mersenne(23) is not prime.
#   isPrimeByFermat2(mersenne(29)) is also true but mersenne(29) is not prime
# 
# mersenne(11), mersenne(23), mersenne(29) and so on are liers
# to the 2^(p-1) mod p == 1 Fermat test.
#
# Fortunately, those are not liers to the 3^(p-1) mod p == 1 Fermat test
#

function mersennePrimesByFermat()
    println("2: $(mersenne(2))")
    for n in primesTo10000
        m = mersenne(n)
        if isPrimeByFermat3(m)
            println("$n: $m")
        end
    end
end

function perfectNumbersByFermat()
    println("2: $(2 * mersenne(2))")
    for n in primesTo10000
        m = mersenne(n)
        if isPrimeByFermat3(m)
            println("$n: $(m << (n-1))")
        end
    end
end

#
# Lucas Lehmer Test
#
function isMersennePrimeIndexNaïvely(p::Integer)::Bool
    if p == 2
        return true
    elseif ! isPrimeBy(p, by=oddPrimesBitmap)
        return false
    else
        m = mersenne(p)
        s = 4
        for i in 1:p-2
            s = (s^2 - 2) % m
        end
        return s == 0
    end
end

function isMersennePrimeIndex(p::Integer)::Bool
    if p == 2
        return true
    elseif ! isPrimeBy(p, by=oddPrimesBitmap)
        return false
    else
        m = mersenne(p)
        s = 4
        for i in 1:p-2
            sq = s * s
            s = (sq & m) + (sq >> p) - 2
            if s >= m
                s -= m
            end
        end
        return s == 0
    end
end

function mersennePrimesByLucasLehmer()
    println("2: $(mersenne(2))")
    for p in primesTo10000
        if isMersennePrimeIndex(p)
            println("$p: $(mersenne(p))")
        end
    end
end


#
# Test Routine
#

using Test

@test length(primesTo10) == 4
@test length(primes5bits) == 11
@test length(primesTo100) == 25
@test length(primesTo1000) == 168
@test length(primesTo10000) == 1229
