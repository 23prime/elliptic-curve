{-# LANGUAGE MonomorphismRestriction #-}

module EllipticCurve.Utils where

import           Data.List
import           System.Random

-- Extended Euclid Algorithm.
-- Forall i (f * si + g * ti == ri)
eea :: (Integral m) => m -> m -> [(m, m, m, m)]
eea f g = loop (f, 1, 0) (g, 0, 1)
  where
    loop (r0, s0, t0) (r1, s1, t1)
      | r2 == 0   = (r0, q, s0, t0) : [(sg * r1, 0, sg * s1, sg * t1)]
      | otherwise = (r0, q, s0, t0) : loop (r1, s1, t1) (r2, s2, t2)
      where
        sg      = signum r1
        (q, r2) = r0 `divMod` r1
        s2      = s0 - s1 * q
        t2      = t0 - t1 * q

-- eea f g => (s, t, r)
-- where f * s + g * t == r == gcd f g
eea' :: (Integral m) => m -> m -> (m, m, m)
eea' f g = (\(w, x, y, z) -> (w, y, z)) $ last $ eea f g

-- Continued Fractions Expansion of (f / g).
continuedFractionsExpansion :: (Integral m) => m -> m -> [m]
continuedFractionsExpansion f g = map (\(w, x, y, z) -> x) $ init $ eea f g


-- Division for taking the smaller Absolute value of Remainder.
divModAbs :: (Integral m) => m -> m -> (m, m)
divModAbs a b
  | abs b >= abs (2 * r) = (q, r)
  | otherwise            = (q + 1, r - b)
  where
    (q, r) = a `divMod` b

-- EEA with absDivMod
eeaAbs :: (Integral m) => m -> m -> [(m, m, m, m)]
eeaAbs f g = loop (f, 1, 0) (g, 0, 1)
  where
    loop (r0, s0, t0) (r1, s1, t1)
      | r2 == 0   = (r0, q, s0, t0) : [(sg * r1, 0, sg * s1, sg * t1)]
      | otherwise = (r0, q, s0, t0) : loop (r1, s1, t1) (r2, s2, t2)
      where
        sg      = signum r1
        (q, r2) = r0 `divModAbs` r1
        s2      = s0 - s1 * q
        t2      = t0 - t1 * q

eeaAbs' :: (Integral m) => m -> m -> (m, m, m)
eeaAbs' f g = (\(w, x, y, z) -> (w, y, z)) $ last $ eeaAbs f g


-- EEA の検算
checkEea :: (Integral m, Random m) => (m -> m -> (m, m, m)) -> Bool
checkEea f
  | False `elem` checkedList = False
  | otherwise                 = True
  where
    checkedList = check f <$> as <*> bs
    as = map (fst . random . mkStdGen) [0..99]
    bs = map (fst . random . mkStdGen) [0..(-99)]
    check f a b
      | let (_, s, t) = f a b
        in  s * a + t * b == gcd a b = True
      | otherwise                    = False

-- EEA の反復回数比較
-- (通常版，調整済み)
eeaFibIteration :: Int -> (Int, Int)
eeaFibIteration a = (length e, length e')
  where
    (s, t) = (fibLog a, fibLog $ a + 1)
    e      = eea s t
    e'     = eeaAbs s t


isDisjointInt :: [Int] -> Bool
isDisjointInt ns = let nl   = length ns
                       gcds = [gcd n m | n <- ns, m <- filter (> n) ns]
                   in all (== 1) gcds && nl * (nl - 1) `div` 2 == length gcds

-- Chinese Remainder Theorem
-- ex.) Find the minimum solution to satisfy the following simultaneous equations.
--        a = 2 mod 5, a = 3 mod 6, a = 5 mod 7
--      -> ans = [(2, 5), (3, 6), (5, 7)]
--      -> chineseRemainder ans
--         => a = 117 (+ 210 m)
chineseRemainder :: [(Int, Int)] -> Int
chineseRemainder eqs
  | isDisjointInt ns = sum (zipWith (*) as $ map (findE n) ns) `mod` n
  | otherwise        = undefined
  where
    as = map fst eqs
    ns = map snd eqs
    n  = product ns
    findE :: Int -> Int -> Int
    findE n n1 = e
      where
        n1' = n `div` n1
        e = let e = (\(x, y, z) -> y) $ eea' n1' n1
            in  n1' * if e < 0
                      then e + n1
                      else e

-- Samples
eqs0 = [(2, 5), (3, 6), (5, 7)] :: [(Int, Int)]
eqs1 = [(2, 3), (3, 5), (5, 8)] :: [(Int, Int)]
eqs2 = [(2, 3), (3, 5), (4, 7), (5, 11)] :: [(Int, Int)]

-------------------------------------------------------------------------------------

isPrime :: (Integral m) => m -> Bool
isPrime 2 = True
isPrime 3 = True
isPrime n
  | n <  2         = False
  | even n         = False
  | n `mod` 3 == 0 = False
  | otherwise      = check n 5 4
  where
    check n m k
      | n < m ^ 2      = True
      | n `mod` m == 0 = False
      | otherwise      = let k' = case k of
                               2 -> 4
                               _ -> 2
                         in  check n (m + k') k'

--primeList = filter isPrime [0..]
primeList n = take n $ filter isPrime [0..]

primeList100e = eratosthenes 100

eratosthenes :: Integer -> [Integer]
eratosthenes m = check m 3 1 [2]
  where
    check :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
    check m p l xs
      | m <= l                        = xs
      | notElem 0 $ map (p `mod`) xs' = check m (p + 2) (l + 1) $ xs ++ [p]
      | otherwise                     = check m (p + 2) l xs
      where
--        xs' = takeWhile (\x -> x ^ 2 <= p) xs
        xs' = takeWhile (\x -> fromInteger x <= sqrt (fromInteger p)) xs

factors :: (Integral m) => m -> [m]
factors n = [x | x <- [1..n], n `mod` x == 0]

factorize :: (Integral m) => m -> [m]
factorize 1 = []
factorize x = let v = factors x !! 1
              in  v : factorize (x `div` v)

isPrimePower :: (Integral m) => m -> Bool
isPrimePower n
  | n < 2                          = False
  | length xs == 1                 = False
  | all (== hxs) xs && isPrime hxs = True
  | otherwise                      = False
    where
      hxs = head xs
      xs  = factorize n

primePower :: (Integral m)
           => m
           -> Maybe (m, Int) -- (prime, power)
primePower n
  | n < 2                          = Nothing
  | length xs == 1                 = Nothing
  | all (== hxs) xs
    && isPrime hxs  = Just (hxs, length xs)
  | otherwise                      = Nothing
    where
      hxs = head xs
      xs  = factorize n

-------------------------------------------------------------------------------------

fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

fib = mkFib 0 1
  where
    mkFib _ y 0 = y
    mkFib x y k = mkFib y (x + y) (k - 1)

fibT = snd . mkFib
  where
    mkFib 0 = (0, 1)
    mkFib n = let (a, b) = mkFib (n - 1)
              in  (b, a + b)

fibLog :: Int -> Integer
fibLog n = fst $ loop (n + 1)
  where
    loop 0 = (0, 1)
    loop n
      | odd n     = let (a, b) = loop (n `div` 2)
                        c      = a + b
                    in (a ^ 2 + b ^ 2, a * b + b * c)
      | otherwise = let (a, b) = loop (n - 1)
                    in (b, a + b)



--continuedFraction
