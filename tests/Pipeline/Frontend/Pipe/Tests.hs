module Pipeline.Frontend.Pipe.Tests where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M
import Control.Monad.State (evalState, execState)
import Control.Monad.Reader (runReaderT)

import Pipeline.Frontend.Pipe
import Pipeline.Frontend.Workflow
import Pipeline.Frontend.Verify

import Pipeline.Frontend.Utils



testWorkflow1 :: Workflow Pipe
testWorkflow1 = do
  readIOTask'   <- registerTask readIOTask
  plus1Task'    <- registerTask plus1Task
  plus1Task''   <- registerTask plus1Task
  plus1Task'''  <- registerTask plus1Task
  showFileTask' <- registerTask (showFileTask "testfiles/testPipeline1.out")

  return $ Pipe $ 
    readIOTask' >>> plus1Task' >>> plus1Task'' >>> plus1Task''' >>> showFileTask' 

testWorkflow2 :: Workflow Pipe
testWorkflow2 = do
  readIOTask' <- registerTask readIOTask
  replicateTask' <- registerTask replicateTask
  zipWithSelf' <- registerTask (zipWithSelf "testfiles/testWorkflow2.1.out")
  zipWith1To100' <- registerTask (zipWith1To100 "testfiles/testWorkflow2.2.out")
  zipWith100To1' <- registerTask (zipWith100To1 "testfiles/testWorkflow2.3.out")

  return $ Pipe (readIOTask' >>> replicateTask' >>> zipWithSelf')
         & Pipe (replicateTask' >>> zipWith1To100')
         & Pipe (replicateTask' >>> zipWith100To1')

tests :: TestTree
tests = testGroup "Frontend" [ nextPIDTests
                             ]

nextPIDTests :: TestTree
nextPIDTests = testGroup "nextPID should"
  [ testCase "return the next pid" $ do
      let pid = evalState nextPID (WorkflowState 3 M.empty)
      pid @?= 4
  , testCase "increment the counter in state" $ do
      let (WorkflowState pid _) = execState nextPID (WorkflowState 5 M.empty)
      pid @?= 6
  ]


