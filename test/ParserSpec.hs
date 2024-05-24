module ParserSpec where

import Code.SS (jparser)
import qualified Data.Text.Lazy as TL
import System.FilePath.Glob (glob)
import Test.Hspec
import Text.S

spec :: Spec
spec = do
  files <- runIO $ glob "files/*.java"
  describe "Try to parse real-world Java files" $ do
    mapM_ test files

test :: String -> SpecWith (Arg Expectation)
test file = do
  let p = jparser <* eof
  res <- runIO $ parseFile p file
  it file $ do
    case res of
      Ok {} -> pure ()
      Err {} -> expectationFailure (TL.unpack . pretty $ res)
