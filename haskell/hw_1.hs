toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = 
    [ if i `mod` 2 == 0 then x * 2 else x | (i, x) <- zip [1..] xs ]

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [ sum (toDigitsRev x) | x <- xs ]

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0
