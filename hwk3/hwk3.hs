import Data.Complex

data Vec a = Vec [a]

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