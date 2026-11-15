module Main (main) where
   
import qualified EraseZero
import qualified Parser
import qualified SemAnalyzer

import Test.HUnit (runTestTT, Test (TestList, TestLabel), Counts (errors, failures))

main :: IO ()
main = do
   counts <- runTestTT tests
   if errors counts + failures counts == 0 
      then pure () 
      else fail "Tests failed"

tests :: Test
tests = TestList 
   [ TestLabel "Parser"       Parser.tests
   , TestLabel "SemAnalyzer"  SemAnalyzer.tests
   , TestLabel "EraseZero"    EraseZero.tests
   ]