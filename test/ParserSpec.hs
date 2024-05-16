module ParserSpec where

import Code.SS (jstmts)
import qualified Data.Text.Lazy as TL
import System.FilePath.Glob (glob)
import Test.Hspec
import Text.S (Result (..), initState, parse, pretty)

spec :: Spec
spec = do
  files <- runIO $ glob "files/*.java"
  describe "Test java-parser with real-world files: " $ do
    mapM_ t files

t :: String -> SpecWith (Arg Expectation)
t filepath = do
  res <- runIO $ do
    f <- readFile filepath
    let state = initState filepath f
    pure $ parse jstmts state
  it filepath $ do
    ( case res of
        Ok _ s -> TL.unpack . pretty $ s
        Err s -> TL.unpack . pretty $ s
      )
      `shouldBe` mempty
