-- range 0 10 5 returns [0, 2, 4, 6, 8]

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