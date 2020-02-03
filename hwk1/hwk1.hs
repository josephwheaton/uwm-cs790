-- todo: Question 1
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
    len = length lst
    idxList = lst `zip` [0..(len - 1)]
    calcReal c = cos(2.0 * pi * c)
    calcImaginary c = sin(2.0 * pi * c)
    rat k n m = fromIntegral k * fromIntegral n / fromIntegral m
    summand k f m ((x, n) : t) = 
      let 
        calc (xi, ni) = (xi * f (rat k ni m))
      in 
        calc (x, n) : map (\(x, n) -> calc (x, n)) t
    summand _ _ _ [] = []
    stepper k f g idxList
      | k < len =
        (sum (summand k f len idxList), sum (summand k g len idxList)) : stepper (k + 1) f g idxList -- ? we want to sum the entire list each time with diff k
      | otherwise = 
        []
  in 
    stepper 0 calcReal calcImaginary idxList

main = 
  do
    let n = 64
    let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
    -- let result = map (\x -> x/n) $ absolute $ dft s -- ! this didn't work
    let result = map (\x -> x/64) $ absolute $ dft s
    print(rd 3 s)
    print(rd 2 result)