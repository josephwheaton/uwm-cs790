-- Tian Zhao, Mar 14 at 4pm

-- Very good. You didn't have to make your own state monad but it is a good exercise. Nicely done.

-- Here is my solution for reference. 

-- makeNoise seed n low high = Vec $ evalState randomSeq $ mkStdGen seed where randomSeq = sequenceA $ take n $ repeat $ state $ randomR (low, high)