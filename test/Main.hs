module Main (main) where
   
import Test.HUnit (runTestTT, Test (TestList, TestLabel), Counts (errors, failures))
import qualified Parser
import qualified SemAnalyzer
import qualified EraseZero

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