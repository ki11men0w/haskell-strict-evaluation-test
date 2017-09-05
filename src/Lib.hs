{-# LANGUAGE BangPatterns #-}
module Lib where

import Control.DeepSeq (force, ($!!))
import Control.Parallel.Strategies
import System.IO
import System.IO.Temp
import Control.Exception (evaluate)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

runEnvironment :: (Handle -> IO String) -> IO ()
runEnvironment action = do
  r <- withSystemTempFile "test2" $ \_ h -> do
                          hPutStr h "qwerty"
                          hSeek h AbsoluteSeek 0
                          action h
  withSystemTempFile "test2" $ \_ h ->
    hPutStrLn h r

woStrictnessApply :: Handle -> IO String
woStrictnessApply = hGetContents

withDeepSeqForce :: Handle -> IO String
withDeepSeqForce h = do
  x <- hGetContents h
  let y = force x
  return y
  
withDeepSeqForceWithWHNF :: Handle -> IO String
withDeepSeqForceWithWHNF h = do
  x <- hGetContents h
  let y = force x
  return $! y
  
withDeepSeqForceEvaluate :: Handle -> IO String
withDeepSeqForceEvaluate h = do
  x <- hGetContents h
  let y = force x
  evaluate y
  
withDeepSeqForceWithBangPattern :: Handle -> IO String
withDeepSeqForceWithBangPattern h = do
  x <- hGetContents h
  let !y = force x
  return y
  
withS2 :: Handle -> IO String
withS2 h = do
  x <- hGetContents h
  return $!! x
  
withStrategiesRdeepseq :: Handle -> IO String
withStrategiesRdeepseq h = do
  x <- hGetContents h
  return (x `using` rdeepseq)
  
withStrategiesRdeepseqWithEvaluate :: Handle -> IO String
withStrategiesRdeepseqWithEvaluate h = do
  x <- hGetContents h
  evaluate (x `using` rdeepseq)
  
withStrategiesRdeepseqWithS1 :: Handle -> IO String
withStrategiesRdeepseqWithS1 h = do
  x <- hGetContents h
  return $! (x `using` rdeepseq)
  
withStrategiesRdeepseqWithBangPattern :: Handle -> IO String
withStrategiesRdeepseqWithBangPattern h = do
  x <- hGetContents h
  let !y = x `using` rdeepseq
  return y
  
withStrategiesRdeepseqWithRunEval :: Handle -> IO String
withStrategiesRdeepseqWithRunEval h = do
  x <- hGetContents h
  return $ runEval $ rdeepseq x
  
withStrategiesRdeepseqWithS2 :: Handle -> IO String
withStrategiesRdeepseqWithS2 h = do
  x <- hGetContents h
  return $!! (x `using` rdeepseq)
  
withStrategiesRseqWithEvaluate :: Handle -> IO String
withStrategiesRseqWithEvaluate h = do
  x <- hGetContents h
  evaluate (x `using` rseq)
      
