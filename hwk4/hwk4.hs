import Data.Complex
import Data.Time
import GHC.Base (liftA2)

-- todo: Reuse type-class instances of Vec, defining Vec as below:
newtype Vec a = Vec {runVec :: [a]}

instance Show a => Show (Vec a) where
  show (Vec a) = "[" ++ (unwords $ show <$> a) ++ "]"

instance Foldable Vec where
  foldr _ z (Vec []) = z
  foldr f z (Vec a) = foldr f z a

-- ! include definitions for each for self
-- todo: 1. Implement instances of Vec for the following type classes:
--  todo: a) Functor class, implement fmap function
-- * Functor: a class for types that can be mapped over.
-- * Functors must preserve identity morphisms (fmap id = id)
-- * Functors preserve composition of morphisms (fmap (f . g) == fmap f . fmap g)
instance Functor Vec where
  fmap f (Vec a) = Vec $ map f a

--  todo: b) Applicative class, implement pure and liftA2 functions
-- * Applicative: a type class intermediate between a functor and a monad, which allow
-- * sequencing of functorial computations without deciding on which computation to
-- * perform on the basis of the result of a previous computation.
instance Applicative Vec where
  pure a = Vec $ repeat a
  (Vec fs) <*> (Vec xs) = Vec $ [f x | f <- fs, x <- xs]
  liftA2 f (Vec a) (Vec b) = f <$> (Vec a) <*> (Vec b)

--  todo: c) Semigroup class, implement the <> operator
-- * Semigroup: a type class that represents a set with an associative binary operation,
-- * this makes semigroups a superset of monoids, there are no other restricitons.
-- * <> must preserve identity
-- * <> must be associative
instance Semigroup (Vec a) where
  (Vec a) <> (Vec b) = Vec $ a ++ b

--  todo: d) Monoid class, implement the mempty function
-- * Monoid: a class for types which have a single most natural operation for
-- * combining values (see definition of semigroup) together with a value which
-- * doesn't do anything when you combine it with others (the identity element).
instance Monoid (Vec a) where
  mempty = Vec []

-- todo: 2. Simplify the Vec instances of Num, Fractional, and Floating using 
-- todo:    fmap, pure, and liftA2 as appropriate
instance Num a => Num (Vec a) where
  Vec a1 + Vec a2 = liftA2 (+) (Vec a1) (Vec a2)
  Vec a1 - Vec a2 = liftA2 (-) (Vec a1) (Vec a2)
  Vec a1 * Vec a2 = liftA2 (*) (Vec a1) (Vec a2)
  negate (Vec a) = negate <$> (Vec a)
  abs (Vec a) = abs <$> (Vec a)
  signum (Vec a) = signum <$> (Vec a)
  fromInteger i = pure $ fromInteger i

instance Fractional a => Fractional (Vec a) where
  Vec a1 / Vec a2 = liftA2 (/) (Vec a1) (Vec a2)
  fromRational r = pure $ fromRational r

instance Floating a => Floating (Vec a) where
  pi = pure pi
  exp (Vec a) = exp <$> (Vec a)
  log (Vec a) = log <$> (Vec a)
  sin (Vec a) = sin <$> (Vec a)
  cos (Vec a) = cos <$> (Vec a)
  asin (Vec a) = asin <$> (Vec a)
  acos (Vec a) = acos <$> (Vec a)
  atan (Vec a) = atan <$> (Vec a)
  sinh (Vec a) = sinh <$> (Vec a)
  cosh (Vec a) = cosh <$> (Vec a)
  asinh (Vec a) = asinh <$> (Vec a)
  acosh (Vec a) = acosh <$> (Vec a)
  atanh (Vec a) = atanh <$> (Vec a)

-- todo: 3. Simplify implementation of dft and fft to leverage Vec Double and 
-- todo:    Vec (Complex Double)
range :: Double -> Double -> Double -> [Double]
range from to count = next from count
  where step = (to - from) / count
        next from count 
          | count <= 0 = []
          | otherwise  = from : next (from+step) (count-1)

absolute :: Vec (Complex Double) -> Vec Double
absolute (Vec []) = (Vec [])
absolute (Vec a) = realPart <$> abs <$> (Vec a)

rd :: Int -> Vec Double -> Vec Double
rd n (Vec a) = 
  f <$> (Vec a)
  where f x = fromIntegral (round (c * x)) / c
        c = 10 ^ n

dft :: [Double] -> Vec (Complex Double)
dft x =
  let n = fromIntegral $ length x
      index = range 0 n n
      xn = x `zip` index
      f k = sum c
            where c = factor <$> xn
                  factor (xi, j) = let y = 2 * pi * j * k / n             
                                   in (xi * cos y) :+ (-xi * sin y)
  in
      Vec $ f <$> index

fft :: [Double] -> Vec (Complex Double)
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
        fft' :: Vec (Complex Double) -> Vec (Complex Double) -> Int -> Vec (Complex Double)
        fft' (Vec []) (Vec []) _ = mempty
        fft' (Vec (a : t1)) (Vec (b : t2)) k -- ? recursive step
          | k >= 0 && k < half =
            Vec [(firstHalf a b)] <> (fft' (Vec t1) (Vec t2) (k+1)) -- ? definition for the first half of the list
          | k >= half && k < len =
            Vec [(secondHalf a b)] <> (fft' (Vec t1) (Vec t2) (k+1)) -- ? definition for the second half of the list
          | otherwise = -- ? k less than zero or greater than or equal to N (len)
            mempty
          where
            firstHalf :: Complex Double -> Complex Double -> Complex Double
            firstHalf a b = ((realPart $ a) + (real k * (realPart $ b)) - (imaginary k * (imagPart $ b))) :+ ((imagPart $ a) + (real k * (imagPart $ b)) + (imaginary k * (realPart $ b)))
            secondHalf :: Complex Double -> Complex Double -> Complex Double
            secondHalf a b = ((realPart $ a) - (real (k - half) * (realPart $ b)) + (imaginary (k - half) * (imagPart $ b))) :+ ((imagPart $ a) - (real (k - half) * (imagPart $ b)) - (imaginary (k - half) * (realPart $ b)))
            real :: Int -> Double
            real k = cos(-2.0 * pi * fromIntegral k / fromIntegral len)
            imaginary :: Int -> Double
            imaginary k = sin(-2.0 * pi * fromIntegral k / fromIntegral len)
      in
        (fft' fevens fodds 0) <> (fft' fevens fodds half) -- ? join prior and latter half of list
  where
    split :: [a] -> ([a], [a])
    split [] = ([], [])
    split [even] = ([even], [])
    split (even:odd:t) = (even:evens, odd:odds) where (evens, odds) = split t

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