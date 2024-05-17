module ParserSpec where

import Code.SS (jstmts)
import System.FilePath.Glob (glob)
import Test.Hspec
import Text.S

spec :: Spec
spec = do
  files <- runIO $ glob "files/*.java"
  describe "Test java-parser with real-world files: " $ do
    mapM_ test files

test :: String -> SpecWith (Arg Expectation)
test file = do
  res <- runIO $ parseFile (jstmts <* eof) file
  it file $ do
    ( case res of
        Ok {} -> pure ()
        Err s -> expectationFailure (take 80 . stateStream $ s)
      )
