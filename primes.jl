primesTo10 = [2,3,5,7]

function isOddPrimeBy(n::Integer; by=[3,5,7])::Bool
    @assert n % 2 != 0 "non even assumed: $n"
    all(by) do p
        n % p != 0
    end
end

primes5bits = [2, 3, 5, 7, 11, 13, 17, 19, 23, 39, 31]

primesTo100 = vcat(primesTo10,
                   [ n for n in 11:2:99
                       if isOddPrimeBy(n, by = [3,5,7])])

primesTo1000 = vcat(primes5bits,
                    [ n for n in 37:2:999
                        if isOddPrimeBy(n, by = @view primes5bits[begin+1:end])
                    ])

primesTo10000 = vcat(primesTo100,
                     [ n for n in 101:2:9999
                         if isOddPrimeBy(n, by = @view primesTo100[begin+1:end])
                     ])

primesTo1000000 = vcat(primesTo1000,
                       [ n for n in 1001:2:999999
                           if isOddPrimeBy(n, by = @view primesTo1000[begin+1:end])
                       ])                

function isPrimeBy(n::Integer; by::Vector{Int} = primesTo10000)::Bool
    all(by) do p
        n % p != 0
    end
end

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

struct OddBitmap <: AbstractVector{Bool}
    bitmap::BitVector

    function OddBitmap(n::Int)::OddBitmap
        bitmap = trues(n)
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
function getindex(xs::OddBitmap, k::Int)
    @assert (k & 0x01) == 1 && k >= 3
    xs.bitmap[(k-1) ÷ 2]
end

function setindex!(xs::OddBitmap, val, k::Int)
    @assert (k & 0x01) == 1 && k >= 3
    xs.bitmap[(k-1) ÷ 2] = val
end

function size(xs::OddBitmap)
    return size(xs.bitmap)
end

# this definition overrides functions axes and eachindex but not size
function axes(xs::OddBitmap)
    map(n->(3:2:(2n+1)), size(xs))
end

function show(io::IO, ::MIME"text/plain", xs::OddBitmap)
    summary(io, xs)
    print(io, xs.bitmap)
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
        throw(ArgumentError("intFactor expects non-zero integer but 0"))
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

function isPrimeByMillerRabin(p::Integer)::Bool
    d = p - 1
end