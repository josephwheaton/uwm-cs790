import Data.Time

-- range 0 10 5 returns [0, 2, 4, 6, 8]

range from to count = map (\x -> from + x * step) [0..count-1] 
  where step = (to - from)/count

absolute = map (\(r, i) -> sqrt(r*r + i*i))

rd n lst = map (\x -> fromIntegral (round $ c * x) / c) lst
  where c = 10^n

dft x =
  let n = fromIntegral $ length x
      index = range 0 n n
      xn = x `zip` index

      f k = (sum r, sum i) 
            where (r, i) = unzip $ map factor xn
                  factor (xi, j) = let y = 2 * pi * j * k / n             
                                   in (xi * cos y, -xi * sin y)
  in
      map f index

fft x  
  | n <= 16 = dft x
  | otherwise = 
     let (even, odd) = split x 
         (e, o) = (fft even, fft odd)
         t = -2 * pi / (fromIntegral n)

         g ((er, ei), (or, oi), k) =  
                   let 
                       y = t * (fromIntegral k) 
                       (fr, fi) = (cos y, sin y)
                       (pr, pi) = (fr * or - fi * oi, fr * oi + fi * or)
                   in
                       ((er + pr, ei + pi), (er - pr, ei - pi))

         (lower, upper) = unzip $ map g $ zip3 e o [0..]
     in
         lower ++ upper
    
  where n = length x
        split [] = ([], [])
        split [a] = ([a], [])
        split (a:b:c) = (a:x, b:y)
           where (x,y) = split c

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