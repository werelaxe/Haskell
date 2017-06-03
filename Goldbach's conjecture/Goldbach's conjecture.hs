
sieve :: [Integer] -> [Integer]
sieve (x : xs) = x : sieve [y | y <- xs, mod y x /= 0]


primes :: [Integer]
primes = sieve [2..]


primesLessThanBuff :: Integer -> Int -> [Integer] -> [Integer]
primesLessThanBuff x index result = do
    let currentPrime = primes !! index
    if (currentPrime > x)
        then result
        else (primesLessThanBuff x (index + 1) result ++ [currentPrime])


primesLessThan :: Integer -> [Integer]
primesLessThan x = primesLessThanBuff x 0 []


explodeNumberBuff :: Integer -> Int -> (Integer, Integer)
explodeNumberBuff evenNumber index = do
    let filteredPrimes = primesLessThan evenNumber
    let currentPrime = filteredPrimes !! index
    let expectedPrime = evenNumber - currentPrime
    if expectedPrime `elem` filteredPrimes
        then (currentPrime, expectedPrime)
        else (explodeNumberBuff evenNumber (index + 1))


explodeNumber :: Integer -> (Integer, Integer)
explodeNumber evenNumber = explodeNumberBuff evenNumber 0


main = do
    evenNumberStr <- getLine
    let evenNumber = read evenNumberStr :: Integer
    if (evenNumber `mod` 2 /= 0)
        then putStrLn "It's an odd number"
        else print(explodeNumber evenNumber)
