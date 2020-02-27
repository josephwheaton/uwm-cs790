import Data.Complex
import Data.Time
import Debug.Trace
import GHC.Base (liftA2)

newtype Vec a = Vec {runVec :: [a]} 

instance Show a => Show (Vec a) where
  show (Vec lst) = '[' : drop 1 (foldr (\e c ->' ': show e ++ c) [']'] lst) 

instance Applicative Vec where
  pure = Vec . repeat 
  (Vec f) <*> (Vec x) = Vec $ map (\(f,x) -> f x) $ zip f x
  liftA2 f (Vec x) (Vec y) = Vec $ map (uncurry f) $ zip x y

instance Functor Vec where
  fmap f (Vec x) = Vec $ map f x

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

         print $ v1 + (pure $ sqrt 2)
         print $ realV v1
         print $ imagV v1
         print $ realV v1 + imagV v2
         print $ sin $ v1 * (pi / 2)
         print $ sum v1
       
