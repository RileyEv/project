module Pipeline.Frontend.Verify.Tests where

import 

import Pipeline.Frontend.Verify (verifyPipe)



verifyTreeTests :: TestTree
verifyTreeTests = testGroup "verifyTree should"
  [ testCase "print types" $ do
      print "hello tests!"
      let (p, s) = buildTree testWorkflow2
      let pidTree = pipeToTree p
      runReaderT (verifyTree pidTree) (tasks s)
  ]
