-- ? In some places I was getting the hang of (x:xs) and didn't use them, instead
-- ? used a stepper function, didn't bother going back and seeing if I could refactor with (x:xs)
-- ? as it was already working

-- todo: Question 1
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

-- todo: Question 2
rd n x = 
  let 
    fix m y = fromIntegral(round $ y * 10^m) / 10^m
  in 
    map (fix n) x

-- todo: Question 3
absolute lst =
  let 
    absComplex (a,b) = sqrt(a^^2 + b^^2)
  in 
    map absComplex lst

-- todo: Question 4
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

-- todo: Testing
main = 
  do
    let n = 64
    let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
    let result = map (\x -> x/n) $ absolute $ dft s -- ! this didn't work
    -- let result = map (\x -> x/64) $ absolute $ dft s
    print(rd 3 s)
    print(rd 2 result)