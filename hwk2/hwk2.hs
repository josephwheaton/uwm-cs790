import Data.Time

-- ? functions from solutions to hwk1
range from to count = next from count
  where step = (to - from) / count
        next from count 
          | count <= 0 = []
          | otherwise  = from : next (from+step) (count-1)

absolute [] = []
absolute ((r,i) : rest) = sqrt(r*r + i*i) : absolute(rest)

rd _ [] = []
rd n (a:b) = f a : rd n b
  where f x = fromIntegral (round (c * x)) / c
        c = 10 ^ n

dft x =
  let n = fromIntegral $ length x
      index = range 0 n n
      xn = x `zip` index
      f [] = []
      f (k:rest) = (sum r, sum i) : f rest
        where (r, i) = unzip $ factor xn
              factor [] = []
              factor ((xi, j) : rest) = (xi * cos y, -xi * sin y) : factor rest
                where y = 2 * pi * j * k / n            
  in
    f index

-- todo: implement split
-- ? splits a list by even and odd indices
split :: [a] -> ([a], [a])
split [] = ([], [])
split [even] = ([even], [])
split (even:odd:t) = (even:evens, odd:odds) where (evens, odds) = split t

-- todo: implement fft
fft :: [Double] -> [(Double, Double)]
fft lst
    | length lst <= 16 = -- ! base case, call dft
      dft(lst)
    | otherwise =
      let
        len = length lst
        half = len `div` 2
        (evens, odds) = split lst
        fevens = fft evens
        fodds = fft odds
        fft' :: [(Double, Double)] -> [(Double, Double)] -> Int -> [(Double, Double)]
        fft' [] [] _ = []
        fft' ((a,b) : t1) ((c,d) : t2) k -- ? recursive step
          | k >= 0 && k < half =
            (firstHalf a b c d) : (fft' t1 t2 (k+1)) -- ? definition for the first half of the list
          | k >= half && k < len =
            (secondHalf a b c d) : (fft' t1 t2 (k+1)) -- ? definition for the second half of the list
          | otherwise = -- ? k less than zero or greater than or equal to N (len)
            []
          where
            -- Ek = (a,b) = (er,ei)
            -- Ok = (c,d) = (or,oi)
            firstHalf :: Double -> Double -> Double -> Double -> (Double, Double)
            firstHalf a b c d = (a + (real k * c) - (imaginary k * d), b + (real k * d) + (imaginary k * c))
            secondHalf :: Double -> Double -> Double -> Double -> (Double, Double)
            secondHalf a b c d = (a - (real (k - half) * c) + (imaginary (k - half) * d), b - (real (k - half) * d) - (imaginary (k - half) * c))
            real :: Int -> Double
            real k = cos(-2.0 * pi * fromIntegral k / fromIntegral len)
            imaginary :: Int -> Double
            imaginary k = sin(-2.0 * pi * fromIntegral k / fromIntegral len)
      in
        (fft' fevens fodds 0) ++ (fft' fevens fodds half) -- ? join prior and latter half of list

main = do 
  let n = 2^8 
  let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n 
  -- print(rd 3 s1)

  start <- getCurrentTime 
  let dft1 = map (\x -> x/n) $ absolute $ dft s1 
  print(rd 2 dft1) 
  end <- getCurrentTime 
  print(length dft1)
  print (diffUTCTime end start)

  start2 <- getCurrentTime 
  let fft1 = map (\x -> x/n) $ absolute $ fft s1 
  print(rd 2 fft1) 
  end2 <- getCurrentTime
  print(length fft1)
  print (diffUTCTime end2 start2)