import Data.Complex
import Data.Time
import Debug.Trace
import GHC.Base (liftA2)

newtype Vec a = Vec {runVec :: [a]} 

instance Show a => Show (Vec a) where
  show (Vec lst) = '[' : drop 1 (foldr (\e c ->' ': show e ++ c) [']'] lst) 

instance Functor Vec where
  fmap f (Vec x) = Vec $ map f x

instance Applicative Vec where
  pure = Vec . repeat 
  (Vec f) <*> (Vec x) = Vec $ map (uncurry ($)) $ zip f x
  liftA2 f (Vec x) (Vec y) = Vec $ map (uncurry f) $ zip x y

instance Num a => Num (Vec a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger x = pure $ fromInteger x

instance (Floating a) => Fractional (Vec a) where
  (/) = liftA2 (/)
  fromRational x = pure $ fromRational x

instance (Floating a) => Floating (Vec a) where
  pi = pure pi
  exp = fmap exp 
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh 
  atanh = fmap atanh
   
instance Foldable Vec where
  foldr f c (Vec a) = foldr f c a

imagV :: Num a => Vec a -> Vec (Complex a)
imagV (Vec a) = Vec $ map (0:+) a

realV :: Num a => Vec a -> Vec (Complex a)
realV (Vec a) = Vec $ map (:+0) a

instance Semigroup (Vec a) where
  Vec a <> Vec b = Vec $ a++b

instance Monoid (Vec a) where
  mempty = Vec []
  

--------- DFT stuff

range :: Double -> Double -> Double -> [Double]
range from to count = map (\x -> from + x * step) [0..count-1] 
  where step = (to - from)/count

absolute :: Vec (Complex Double) -> Vec Double
absolute = fmap (\(r:+i) -> sqrt(r*r + i*i))

rd :: Int -> Vec Double -> Vec Double
rd n = fmap (\x -> fromIntegral (round $ c * x) / c) 
  where c = 10^n

dft :: [Double] -> Vec (Complex Double)
dft x = 
   let bigN = fromIntegral $ length x -- length of x
       n = Vec [0 .. bigN-1]              -- index
       x' = realV $ Vec x
       f = \k -> sum $ x' * exp (imagV $ n * pure (-2*pi*k / bigN))
   in fmap f n

trace_fft m x = trace (m ++ ": " ++ (show $ rd 2 $ absolute x)) x -- debugging helper to print fft results

fft :: [Double] -> Vec (Complex Double)
fft x  
  | n <= 16 = dft x
  | otherwise = 
     let (even, odd) = split x 
         -- (e, o) = (trace_fft "even" $ fft even, trace_fft "odd" $ fft odd) -- debugging example
         (e, o) = (fft even, fft odd) 

         t = -2 * pi / (fromIntegral n)
         k = Vec [0..]

         p = exp( imagV $ k * pure t ) * o
     in
         (e + p) <> (e - p)
    
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
         let dft1 = fmap (/n) $ absolute $ dft s1
         print(sum $ rd 2 dft1)
         end <- getCurrentTime
         print (diffUTCTime end start)

         start2 <- getCurrentTime
         let fft1 = fmap (/n) $ absolute $ fft s1
         print(sum $ rd 2 fft1)
         end2 <- getCurrentTime
         print (diffUTCTime end2 start2)
