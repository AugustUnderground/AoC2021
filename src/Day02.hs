module Day02 (solve) where

solve :: IO ()
solve = do
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    silver = 0
    gold   = 0
