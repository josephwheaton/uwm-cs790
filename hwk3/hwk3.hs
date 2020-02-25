import Data.Complex

-- ? implement instances of the following type classes for the data type below
data Vec a = Vec [a]

-- todo: 1. Show class
instance Show a => Show (Vec a) where
  show (Vec a) = "[" ++ (unwords $ map show a) ++ "]"

-- todo: 2. Num class
instance Num a => Num (Vec a) where
  Vec a1 + Vec a2 = Vec $ zipWith (+) a1 a2
  Vec a1 - Vec a2 = Vec $ zipWith (-) a1 a2
  Vec a1 * Vec a2 = Vec $ zipWith (*) a1 a2
  negate (Vec a) = Vec $ map negate a
  abs (Vec a) = Vec $ map abs a
  signum (Vec a) = Vec $ map signum a
  fromInteger i = Vec $ repeat $ fromInteger i

-- todo: 3. Fractional class
instance Fractional a => Fractional (Vec a) where
  Vec a1 / Vec a2 = Vec $ zipWith (/) a1 a2
  fromRational r = Vec $ repeat $ fromRational r

-- todo: 4. Floating class
instance Floating a => Floating (Vec a) where
  pi = Vec $ repeat pi
  exp (Vec a) = Vec $ map exp a
  log (Vec a) = Vec $ map log a
  sin (Vec a) = Vec $ map sin a
  cos (Vec a) = Vec $ map cos a
  asin (Vec a) = Vec $ map asin a
  acos (Vec a) = Vec $ map acos a
  atan (Vec a) = Vec $ map atan a
  sinh (Vec a) = Vec $ map sinh a
  cosh (Vec a) = Vec $ map cosh a
  asinh (Vec a) = Vec $ map asinh a
  acosh (Vec a) = Vec $ map acosh a
  atanh (Vec a) = Vec $ map atanh a

-- todo: 5. Foldable class
instance Foldable Vec where
  foldr _ z (Vec []) = z
  foldr f z (Vec a) = foldr f z a

-- ? implement the following functions
-- todo: 1. pure
pure' :: Num a => a -> Vec a
pure' a = Vec $ repeat a

-- todo: 2. realV
realV :: Num a => Vec a -> Vec (Complex a)
realV a = Vec $ foldr (\x xs -> (x :+ 0):xs) [] a

-- todo: 3. imagV
imagV :: Num a => Vec a -> Vec (Complex a)
imagV a = Vec $ foldr (\x xs -> (0 :+ x):xs) [] a

main = do
  let v1 = Vec [1,2,3]
  let v2 = Vec [2,3,4]
  let v3 = Vec [-10,0,10]
  print $ v1 + v2
  print $ v1 - v2
  print $ v1 * v2
  print $ v1 / v2
  print $ negate v1
  print $ signum v3
  print $ abs v3
  print $ v1 + 10
  print $ v2 + 1.2
  print $ v1 + (pure' $ sqrt 2)
  print $ realV v1
  print $ imagV v1
  print $ realV v1 + imagV v2
  print $ sin $ v1 * (pi / 2)
  print $ sum v1