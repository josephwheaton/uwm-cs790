import Data.Complex
import GHC.Base (liftA2)

-- todo: Reuse type-class instances of Vec, defining Vec as below:

newtype Vec a = Vec {runVec :: [a]}

-- ! include definitions for each for self
-- todo: 1. Implement instances of Vec for the following type classes:
--  todo: a) Functor class, implement fmap function



--  todo: b) Applicative class, implement pure and liftA2 functions



--  todo: c) Semigroup class, implement the <> operator



--  todo: d) Monoid class, implement the mempty function



-- todo: 2. Simplify the Vec instances of Num, Fractional, and Floating using 
-- todo:    fmap, pure, and liftA2 as appropriate



-- todo: 3. Simplify implementation of dft and fft to leverage Vec Double and 
-- todo:    Vec (Complex Double)

range :: Double -> Double -> Double -> [Double]


absolute :: Vec (Complex Double) -> Vec Double


rd :: Int -> Vec Double -> Vec Double


dft :: [Double] -> Vec (Complex Double)


fft :: [Double] -> Vec (Complex Double)



-- todo: Testing
{-
main = do
  let n = 2^8
  let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n
  start <- getCurrentTime
  let dft1 = fmap (/n) $ absolute $ dft s1
  print(rd 2 dft1)
  end <- getCurrentTime
  print (diffUTCTime end start)
  start2 <- getCurrentTime
  let fft1 = fmap (/n) $ absolute $ fft s1
  print(rd 2 fft1)
  end2 <- getCurrentTime
  print (diffUTCTime end2 start2)
-}