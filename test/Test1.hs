module Test1 (specTest1) where

import System.IO.Error
import Control.Exception
import Test.Hspec
import Data.List (isInfixOf)
import Lib

delayedReadOnClosedHandle :: Selector IOException
delayedReadOnClosedHandle e = isIllegalOperation e && ("closed handle" `isInfixOf` show e)

specTest1 :: Spec
specTest1 = do
  context "Testing strictness" $ do
    it "without strctness should end with error" $
       runEnvironment woStrictnessApply `shouldThrow` delayedReadOnClosedHandle
    context "with Control.DeepSeq" $ do
      it "force only should end with error" $
         runEnvironment withDeepSeqForce `shouldThrow` delayedReadOnClosedHandle
      it "force and $!" $
         runEnvironment withDeepSeqForceWithWHNF 
      it "force and Control.Exception.evaluate" $
         runEnvironment withDeepSeqForceEvaluate
      it "force and bang pattern" $
         runEnvironment withDeepSeqForceWithBangPattern
      it "$!!" $
         runEnvironment withS2 
    context "with Control.Parallel.Strategies" $ do
      it "rdeepseq should end with error" $
         runEnvironment withStrategiesRdeepseq `shouldThrow` delayedReadOnClosedHandle
      it "rdeepseq && evaluate" $
         runEnvironment withStrategiesRdeepseqWithEvaluate
      it "rdeepseq && $!" $
         runEnvironment withStrategiesRdeepseqWithS1
      it "rdeepseq && $!!" $
         runEnvironment withStrategiesRdeepseqWithS2
      it "rdeepseq && bang pattern" $
         runEnvironment withStrategiesRdeepseqWithBangPattern
      it "rdeepseq && runEval" $
         runEnvironment withStrategiesRdeepseqWithRunEval
      it "rseq && evaluate should end with error" $
         runEnvironment withStrategiesRseqWithEvaluate `shouldThrow` delayedReadOnClosedHandle
