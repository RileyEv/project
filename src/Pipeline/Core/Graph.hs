module Pipeline.Core.Graph (
  TaskTreeF(..),
  DataTreeF(..),
  FList(..),
) where

import Pipeline.Core.IFunctor (IFix2(..))
import Pipeline.Core.Task (TaskF)
import Pipeline.Core.DataStore (Apply, DataSource', HAppendListR, HList)


data TaskTreeF f i o where
  TBranchF :: (fas ~ Apply fs as)
          => IFix2 TaskF fas '[g b]
          -> FList f '[g b] hcs
          -> TaskTreeF f (Apply fs as) hcs
  TLeafF :: (fas ~ Apply fs as, DataSource' fs as fas, DataSource' '[g] '[b] '[g b]) => IFix2 TaskF fas '[g b] -> TaskTreeF f fas '[g b]


data DataTreeF f i o where
  DBranchF :: (DataSource' fs as (Apply fs as)) => HList (Apply fs as) -> FList f (Apply fs as) gbs -> DataTreeF f (Apply fs as) gbs
  DLeafF :: (fas ~ Apply fs as, DataSource' fs as fas) => HList fas -> DataTreeF f fas fas

data FList (f :: [*] -> [*] -> *) (is :: [*]) (xs :: [*]) where
  FCons :: f is xs -> FList f is ys -> FList f is (HAppendListR xs ys)
  FNil  :: FList f is '[]
