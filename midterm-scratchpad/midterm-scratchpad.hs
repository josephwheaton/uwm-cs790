reorder :: [Int] -> (Bool, [Int])
reorder [] = (False, [])
reorder [x] = (False, [x])
reorder (a:b:rest) =
  if a > b then (True, (b:(snd $ reorder(a:rest))))
  else (fst $ (reorder (b:rest)), a : (snd $ reorder (b:rest)))

bubble_sort :: [Int] -> [Int]
bubble_sort ns =
  let (reordered, result) = reorder ns
  in
    if reordered 
    then bubble_sort result
    else result

maxList [x] = x
maxList (x:xs)
  | (maxList xs) > x = maxList xs
  | otherwise = x

maxList' _ [x] = x
maxList' f (x:xs) =
  if f (maxList' f xs) x then x
  else maxList' f xs

main = do
  print(bubble_sort [3,1,5,0,2])
  print(maxList' (<) [2,4,1,11,9])