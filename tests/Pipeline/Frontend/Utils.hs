module Pipeline.Frontend.Utils where

import Pipeline.Core.Task
import Pipeline.Core.DataStore

-- Helpful example tasks that can be used.

readIOTask :: Task IOStore String VariableStore Int
readIOTask = functionTask (read :: String -> Int) Empty

plus1Task :: Task VariableStore Int VariableStore Int
plus1Task = functionTask (+ (1 :: Int)) Empty

showFileTask :: FilePath -> Task VariableStore Int FileStore String
showFileTask f = functionTask (show :: Int -> String) (FileStore f)
  
replicateTask :: Task VariableStore Int VariableStore [Int]
replicateTask = functionTask (replicate 100) Empty

zipWithSelf :: FilePath -> Task VariableStore [Int] CSVStore [(Int, Int)]
zipWithSelf f = functionTask (\xs -> zip xs xs) (CSVStore f)

zipWith1To100 :: FilePath -> Task VariableStore [Int] CSVStore [(Int, Int)]
zipWith1To100 f = functionTask (zip [1..100]) (CSVStore f)

zipWith100To1 :: FilePath -> Task VariableStore [Int] CSVStore [(Int, Int)]
zipWith100To1 f = functionTask (zip [100, 99..1]) (CSVStore f)
