-- todo: Question 1
-- ! range :: (Fractional a, Integral b) => a -> a -> b -> [a]
range from to count =
  let 
    total = fromIntegral(count)
    step = (to - from) / total
    stepper currentCount = 
      if 
        currentCount < count 
      then 
        from + fromIntegral(currentCount) * step : stepper (currentCount + 1) 
      else 
        []
  in 
    stepper 0

-- todo: Question 2
-- ! rd :: (Integral a, RealFrac b) => a -> [b] -> [b]
rd n x = 
  let 
    fix m y = fromIntegral(round $ y * 10^m) / 10^m
  in 
    map (fix n) x

-- todo: Question 3
-- ! absolute :: Num a => [(a, a)] -> [a]
absolute lst =
  let 
    absComplex (a,b) = sqrt(a^^2 + b^^2)
  in 
    map absComplex lst

-- todo: Question 4
-- ! dft :: [Num] a => [a] -> [(a, a)]
-- ? x is value of elem of inner foldl
-- ? k is index of elem of outer map
-- ? n is index of elem of inner foldl
-- ? m is length of the list, number of elems

dft lst = 
  let 
    len = length lst
    indexedLeftList = flip zip [1,0..] $ fst $ unzip $ lst
    indexedRightList = flip zip [1,0..] $ snd $ unzip $ lst 
    calcReal c = cos(2 * pi * c)
    calcImaginary c = sin(2 * pi * c)
    rat k n m = fromIntegral k * fromIntegral n / fromIntegral m
    inner k f m ((x, n) : t) = 
      let 
        calc (xi, ni) = (xi * f (rat k ni m))
      in 
        calc (x, n) : map (\(x, n) -> calc (x, n)) t
    inner _ _ _ [] = []
    stepper k f idxList
      | k < len =
        (foldl (+) 0 (inner k f len idxList)) : stepper (k + 1) f idxList -- ? we want to sum the entire list each time with diff k
      | otherwise = 
        []
  in 
    -- stepper 0 calcReal indexedLeftList
    stepper 0 calcReal indexedLeftList `zip` stepper 0 calcImaginary indexedRightList



-- ? just zip left and right lists
-- dft reals = zip leftLst rightLst

{- main = do
let n = 64
let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
let result = map (\x -> x/n) $ absolute $ dft s
print(rd 3 s)
print(rd 2 result)
-}
