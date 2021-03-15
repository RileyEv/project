module Pipeline.Frontend.GraphTransformation where

import Pipeline.Core.Modular ((:+:)(..))
import Pipeline.Core.IFunctor (IFix2(..), IFunctor2(..), icata2)
import Pipeline.Core.Task (TaskF(..))
import Pipeline.Core.Graph (TaskTreeF(..))

import Pipeline.Frontend.Circuit (Replicate(..), Id(..))



makeTree :: IFix2 (Replicate :+: TaskF :+: Id) inputs outputs -> IFix2 TaskTreeF inputs outputs
makeTree = icata2 makeTreeAlg 

class IFunctor2 iF => MakeTreeAlg iF where
  makeTreeAlg :: iF (IFix2 TaskTreeF) inputs outputs -> (IFix2 TaskTreeF) inputs outputs

instance (MakeTreeAlg iF, MakeTreeAlg iG) => MakeTreeAlg (iF :+: iG) where
  makeTreeAlg (L x) = makeTreeAlg x
  makeTreeAlg (R y) = makeTreeAlg y


-- TODO: Add definitions
instance MakeTreeAlg Replicate where
  makeTreeAlg = undefined

instance MakeTreeAlg TaskF where
  makeTreeAlg = undefined

instance MakeTreeAlg Id where
  makeTreeAlg = undefined
