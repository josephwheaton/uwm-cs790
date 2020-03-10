{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Data.Complex
import Debug.Trace
import GHC.Base (liftA2)
import System.Random

-- ! Begin hwk5 definitions
newtype Vec a = Vec {runVec :: [a]}

newtype Reader r a = Reader {runReader :: r -> a} deriving (Functor, Applicative, Monad)

class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where
  ask = Reader id
  local f m = Reader $ runReader m . f

type Signal = Vec (Complex Double)

instance Show a => Show (Vec a) where
  show (Vec lst) = "[" ++ drop 1 lst' ++ "]"
    where lst' = mconcat $ map (\x -> " " ++ show x) lst

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
  atanh = fmap atan

instance Foldable Vec where
  foldr f c (Vec a) = foldr f c a

imagV :: Num a => Vec a -> Vec (Complex a)
imagV (Vec a) = Vec $ map (0:+) a

realV :: Num a => Vec a -> Vec (Complex a)
realV (Vec a) = Vec $ map (:+0) a

instance Semigroup (Vec a) where Vec a <> Vec b = Vec $ a++b

instance Monoid (Vec a) where mempty = Vec []

range :: Double -> Double -> Double -> [Double]
range from to count = map (\x -> from + x * step) [0..count-1]
  where step = (to - from) / count

absolute :: Vec (Complex Double) -> Vec Double
absolute = fmap (\(r:+i) -> sqrt(r*r + i*i))

rd :: Int -> Vec Double -> Vec Double
rd n = fmap (\x -> fromIntegral (round $ c * x) / c)
  where c = 10^n

length' :: Vec a -> Int
length' (Vec x) = length x

split' :: [a] -> ([a], [a])
split' [] = ([], [])
split' [a] = ([a], [])
split' (a:b:c) = (a:x, b:y)
  where (x,y) = split' c

twiddle :: Double -> Double -> Signal
twiddle n k = 
  let 
    ns = range 0 n n
    y = -2.0 * pi * k / n
  in
    Vec $ (\ni -> exp(0.0 :+ (ni * y))) <$> ns

dft :: Signal -> Signal
dft v = ((\tf -> sum (v * tf)) <$> tfs)
  where 
    n :: Double
    n = fromIntegral $ length' v
    ns :: Vec Double
    ns = Vec $ range 0 n n
    tfs :: Vec (Signal)
    tfs = (twiddle n) <$> ns

idft :: Signal -> Signal
idft v = (/(n :+ 0.0)) <$> conjugate <$> dft (conjugate <$> v)
  where
    n :: Double
    n = fromIntegral $ length' v

mask :: Int -> Int -> Signal
mask freq n = 
  realV $ Vec $ one ++ zero ++ one
    where one = (take freq $ repeat 1)
          zero = (take (n - freq * 2) $ repeat 0)

low_pass' :: Int -> Signal -> Signal
low_pass' freq v = 
  let n = length' v 
  in idft (dft v * mask freq n)

twiddle' :: Double -> Signal
twiddle' n = 
  let
    m = n / 2
    ns = range 0 m m
    y = -2.0 * pi / n
  in Vec $ (\ni -> exp(0.0 :+ (ni * y))) <$> ns

fft' :: Signal -> Signal -> Signal
fft' v tf
  | n == 1 = 
    v
  | otherwise =
    let 
      (es, os) = split' (runVec v)
      (even, odd) = (Vec es, Vec os)
      tf' = Vec $ fst $ split' (runVec tf)
      (e, o) = (fft' even tf', fft' odd tf')
      p = tf * o
    in
      (e + p) <> (e - p)
  where 
    n = length' v

ifft' :: Signal -> Signal -> Signal
ifft' v tf = (/(n :+ 0.0)) <$> conjugate <$> fft' (conjugate <$> v) tf
  where n = fromIntegral $ length' v

low_pass'' :: Int -> Signal -> Signal
low_pass'' freq v = 
  let n = length' v 
      tf = twiddle' $ fromIntegral n
  in ifft' ((fft' v tf) * mask freq n) tf

ntf :: MonadReader Signal m => m a -> m a
ntf = local (\ctx -> Vec $ fst $ split' (runVec $ ctx))

fft :: Signal -> Reader Signal Signal
fft v = do
  let (e, o) = split' (runVec v)
      (even, odd) = (Vec e, Vec o)
  tf <- ask
  e  <- ntf $ fft even
  o  <- ntf $ fft odd
  return $ 
    if (length' v) == 1 then v
    else
      let
        p = tf * o
      in 
        (e + p) <> (e - p)

ifft :: Signal -> Reader Signal Signal
ifft v = do
  forward <- fft (conjugate <$> v)
  return $
    let
      n = fromIntegral $ length' v
    in
      (/(n :+ 0.0)) <$> conjugate <$> forward

low_pass :: Int -> Signal -> Signal
low_pass freq v = y
  where
    y = (runReader x) (twiddle' $ fromIntegral $ length' v)
    x = do 
          let n = length' v
              m = mask freq n
          forward <- fft v
          inverse <- ifft (forward * m)
          return inverse
-- ! End hwk5 definitions

-- todo: 1. Add makeNoise function 
-- todo: 2. Use state monad to keep track of generator state
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f p = State $ \s -> let (a, s') = runState p s in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (x, s)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  p <*> q = State $  \s ->
    let (f, s1) = runState p s
        (a, s2) = runState q s1
    in  (f a, s2)                       

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  p >>= k = State $ \s0 ->
    let (x1, s1) = runState p      $ s0
        (x2, s2) = runState (k x1) $ s1
    in  (x2, s2)

class Monad m => MonadState s (m :: * -> *) | m -> s where
  get :: m s
  put :: s -> m ()
  state :: (s -> (a, s)) -> m a

instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  put s = State $ \_ -> ((), s)
  state = State

getRandoms :: (Random a, RandomGen g) => Int -> (a, a) -> State ([a], g) [a]
getRandoms 0 _      = do
  (randoms, _) <- get
  return $ reverse randoms
getRandoms k (l, h) = do
  (vs, cg) <- get
  let (v, ng) = randomR (l, h) cg
  put (v:vs, ng)
  getRandoms (k-1) (l, h)

evalState :: State g a -> g -> a
evalState act = fst . runState act

execState :: State g a -> g -> g
execState act = snd . runState act

makeNoise :: Random a => Int -> Int -> a -> a -> Vec a
makeNoise seed n low high =
  Vec $ evalState (getRandoms n (low, high)) ([], mkStdGen seed)

-- todo: 3. Testing

main = do
  let n' = 2^8
  let noise = makeNoise 0 n' (-1.0) 1.0
  let n = fromIntegral n'
  let s = fmap (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ Vec $ range 0 1 n
  let s1 = s + noise

  -- ? noisy signal
  print(rd 2 s1)

  -- ? noisy frequency
  print(rd 2 $ fmap (/n) $ absolute $ dft $ realV s1)

  -- ? filtered signal (dft)
  print(rd 3 $ fmap (\(r:+_) -> r) $ low_pass' 15 $ realV s1)

  -- ? filtered signal (fft)
  print(rd 2 $ fmap (\(r:+_) -> r) $ low_pass 15 $ realV s1)