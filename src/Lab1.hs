module Lab1 where

{- Lab 1
   Date: 2024-10-05
   Authors: Bharath Ravichandran, Giacomo Guidotto
   Lab group: 34
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k - 1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1

-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k = product (replicate (fromIntegral k) n)

power1' :: Integer -> Integer -> Integer
power1' n k = product [n | _ <- [1 .. k]]

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k
  | even k = power2 (n * n) (div k 2)
  | otherwise = n * power2 n (k - 1)

-- D -------------------------
{-
    Test cases:
    case 1: power 2 3 = 8
    case 2: power 3 2 = 9
    case 3: power 4 0 = 1
    case 4: power 0 4 = 0
 -}

--
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power1 n k == power2 n k

--
powerTest :: Bool
powerTest =
  and
    [ prop_powers 2 3,
      prop_powers 3 2,
      prop_powers 4 0,
      prop_powers 0 4
    ]

--
prop_powers' :: Integer -> Integer -> Property
prop_powers' n k = k >= 0 ==> power n k == power1 n k && power1 n k == power2 n k
