import Data.Time

-- ? functions from hwk1
range from to count =
  let 
    total = round(count) -- ? this is done because of type inference in the main do which inferred count as an integer here and a realfrac elsewhere
    step = (to - from) / count
    stepper currentCount = 
      if 
        currentCount < total
      then 
        from + fromIntegral(currentCount) * step : stepper (currentCount + 1) 
      else 
        []
  in 
    stepper 0

rd n x = 
  let 
    fix m y = fromIntegral(round $ y * 10^m) / 10^m
  in 
    map (fix n) x

absolute lst =
  let 
    absComplex (a,b) = sqrt(a^^2 + b^^2)
  in 
    map absComplex lst

dft :: [Double] -> [(Double, Double)]
dft lst = 
  let 
    len = length lst -- ? save off for convenience
    idxList = lst `zip` [0..(len - 1)] -- ? give list items an index
    calcReal c = cos(2.0 * pi * c)
    calcImaginary c = sin(2.0 * pi * c)
    rat k n m = fromIntegral k * fromIntegral n / fromIntegral m
    summand k f m ((x, n) : t) = -- ? get list of summands
      let 
        calc (xi, ni) = (xi * f (rat k ni m))
      in 
        calc (x, n) : map (\(x, n) -> calc (x, n)) t
    summand _ _ _ [] = []
    stepper k f g idxList -- ? we want to sum the entire list each time with different k
      | k < len =
        (sum (summand k f len idxList), sum (summand k g len idxList)) : stepper (k + 1) f g idxList -- ? sum summands for real and imaginary part, create list of tuples and append k+1 onto it
      | otherwise = 
        []
  in 
    stepper 0 calcReal calcImaginary idxList

-- todo: implement split
-- ? splits a list by even and odd indices
split :: [a] -> ([a], [a])
split lst = 
  let 
    len = length lst
    idxLst = lst `zip` [0..(len - 1)]
    splitter [] (evens, odds) = (evens, odds)
    splitter ((val, idx) : t) (evens, odds)
      | idx `mod` 2 == 0 = 
        splitter t (val:evens, odds)
      | otherwise =  
        splitter t (evens, val:odds)
  in splitter idxLst ([], [])

-- todo: implement fft
fft :: [Double] -> [(Double, Double)]
fft lst
    | len <= 16 = -- ! base case, call dft
      dft(lst)
    | otherwise =
      let
        splitLst = split lst
        evens = fst splitLst
        odds = snd splitLst
        real :: Int -> Double
        real k = cos(-2.0 * pi * fromIntegral k / fromIntegral len)
        imaginary :: Int -> Double
        imaginary k = sin(-2.0 * pi * fromIntegral k / fromIntegral len)
        fft' :: [(Double, Double)] -> [(Double, Double)] -> Int -> [(Double, Double)]
        fft' ((a,b) : t1) ((c,d) : t2) k
          | k < len `div` 2 =
            firstHalf a b c d : (fft' t1 t2 (k+1))
          | k < len && k >= len `div` 2 =
            secondHalf a b c d : (fft' t1 t2 (k+1))
          | otherwise = []
          where
            u = real k
            v = imaginary k
            firstHalf :: Double -> Double -> Double -> Double -> (Double, Double)
            firstHalf a b c d = (a + (u * c) - (v * d), b + (u * d) + (v * c))
            secondHalf :: Double -> Double -> Double -> Double -> (Double, Double)
            secondHalf a b c d = (a - (u * c) + (v * d), b - (u * d) - (v * c))
      in
        fft' (fft evens) (fft odds) 0
    where len = length lst

main = do 
  let n = 2^8 
  let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n 
  -- print(rd 3 s1)

  start <- getCurrentTime 
  let dft1 = map (\x -> x/n) $ absolute $ dft s1 
  print(rd 2 dft1) 
  end <- getCurrentTime 
  print (diffUTCTime end start)

  start2 <- getCurrentTime 
  let fft1 = map (\x -> x/n) $ absolute $ fft s1 
  print(rd 2 fft1) 
  end2 <- getCurrentTime 
  print (diffUTCTime end2 start2)
