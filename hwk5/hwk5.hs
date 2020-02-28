import Data.Complex
import Data.Time
import GHC.Base (liftA2)
newtype Vec a = Vec {runVec :: [a]}

-- todo: 1. We will use the tyupe alias Signal
type Signal = Vec (Complex Double)

-- todo: 2. Provided type class instances
-- ! replaced provided show with my own implementation
instance Show a => Show (Vec a) where
  show (Vec a) = "[" ++ (unwords $ show <$> a) ++ "]"

instance Functor Vec where
  fmap f (Vec a) = Vec $ map f a

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
  fromRational x = pure $ fromRational

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
  atanh = fmap atan

instance Foldable Vec where 
  foldr f c (Vec a) = foldr f c a

imagV :: Num a => Vec a -> Vec (Complex a) 
imagV (Vec a) = Vec $ map (0:+) a

realV :: Num a => Vec a -> Vec (Complex a) 
realV (Vec a) = Vec $ map (:+0) a

instance Semigroup (Vec a) where Vec a <> Vec b = Vec $ a++b

instance Monoid (Vec a) where mempty = Vec []

-- todo: 3. Provided auxiliary functions
range :: Double -> Double -> Double -> [Double] 
range from to count = map (\x -> from + x * step) [0..count-1] 
  where step = (to - from)/count

absolute :: Vec (Complex Double) -> Vec Double 
absolute = fmap (\(r:+i) -> sqrt(r*r + i*i))

rd :: Int -> Vec Double -> Vec Double 
rd n = fmap (\x -> fromIntegral (round $ c * x) / c) 
  where c = 10^n

length’ (Vec x) = length x

-- todo: 4. Implement a function to calculate the twiddle factor for DFT
-- todo: 4. a) implement twiddle
twiddle :: Double -> Double -> Signal


-- todo: 4. b) use twiddle to implement DFT
dft :: Signal -> Signal


-- todo: 4. c) implement inverse DFT
idft :: Signal -> Signal


-- todo: 5. Implement a low-pass filter using DFT and inverse DFT
-- todo: 5. a) implement mask
mask :: Int -> Int -> Signa


-- todo: 5. b) implement low_pass'
low_pass’ :: Int -> Signal -> Signal


-- todo: 6. Implement FFT and IFFT, reusing the twiddle factor(s)
-- todo: 6. a) fft
fft :: Signal -> Reader Signal Signal


-- todo: 6. b) ifft
ifft :: Signal -> Reader Signal Signal


-- todo: 6. c) Implement low_pass using the new fft and ifft
low_pass :: Int -> Signal -> Signal


-- todo: Testing
main = do 
  let n = fromIntegral 2^8 
  let s1 = fmap (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ Vec $ range 0 1 n 
  print(rd 3 s1)
  print(rd 3 $ fmap (\(r:+_) -> r) $ low_pass’ 15 $ realV s1)
  print(rd 3 $ fmap (\(r:+_) -> r) $ low_pass 15 $ realV s1)
