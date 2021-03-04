module Pipeline.Frontend.Verify where

import Pipeline.Core.IFunctor (IFix(..), IFix4(..))
import Pipeline.Core.Graph (TreeF(..))
import Pipeline.Core.Task (TaskWrap(..))
import Pipeline.Frontend.PID (PID)
import Pipeline.Frontend.Pipe (Pipe(..), ChainF(..), Chain)

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans (lift)

import qualified Data.Map as M (Map, lookup)
import qualified Data.Set as S (Set, member, insert)
import Data.Typeable (typeOf, typeRepArgs)


verifyPipe :: Pipe -> StateT (S.Set PID) IO ()
verifyPipe (Pipe p) = verifyFirstChain p
  where 
    verifyFirstChain :: Chain f a g b -> StateT (S.Set PID) IO ()
    verifyFirstChain (IIn4 (ProcessF pid)) = do
      s <- get
      if pid `S.member` s
        then error "Not a valid chain"
        else put (pid `S.insert` s)
    verifyFirstChain (IIn4 (JoinF x y)) = do
      verifyFirstChain x
      verifyFirstChain y
verifyPipe (And x y) = do
  verifyPipe x
  verifyOtherPipes y
  where
    verifyOtherPipes :: Pipe -> StateT (S.Set PID) IO ()
    verifyOtherPipes (Pipe p) = verifyChain p
    verifyOtherPipes (And x y) = undefined
    verifyChain :: Chain f a g b -> StateT (S.Set PID) IO ()
    verifyChain (IIn4 (ProcessF pid)) = do
      s <- get
      if pid `S.member` s
        then return ()
        else error "First item in a chain needs to be in a previous chain"
    verifyChain (IIn4 (JoinF x y)) = do
      verifyChain x
      verifyOtherChains y
    verifyOtherChains :: Chain f a g b -> StateT (S.Set PID) IO ()
    verifyOtherChains (IIn4 (ProcessF pid)) = do
      s <- get
      if pid `S.member` s
        then error "Only first process cannot exist in a previous chain"
        else put (pid `S.insert` s)
    verifyOtherChains (IIn4 (JoinF x y)) = do
      verifyOtherChains x
      verifyOtherChains y
        


verifyTree :: (IFix TreeF) PID -> ReaderT (M.Map PID TaskWrap) IO ()
verifyTree (IIn (TreeF _ [])) = return ()
verifyTree (IIn (TreeF x cs)) = do
  forM_ cs f
  forM_ cs verifyTree
  where
    f :: (IFix TreeF) PID -> ReaderT (M.Map PID TaskWrap) IO ()
    f (IIn (TreeF y _)) = do
      m <- ask
      let xf = M.lookup x m
      let yf = M.lookup y m
      verifyNodeType xf yf

verifyNodeType :: Maybe TaskWrap -> Maybe TaskWrap -> ReaderT (M.Map PID TaskWrap) IO ()
verifyNodeType (Just (TaskWrap t)) (Just (TaskWrap t')) = do
  let xArgs = typeRepArgs (typeOf t)
  let yArgs = typeRepArgs (typeOf t')
  if drop 2 xArgs == take 2 yArgs
    then lift $ print "They Match!"
    else error "Types do not match on task" 
verifyNodeType _ _ = error "Not in the map, should have failed before this?"


verify :: (IFix TreeF) PID -> M.Map PID TaskWrap -> IO ()
verify t = runReaderT (verifyTree t)
