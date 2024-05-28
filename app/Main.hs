{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad ((>=>))
import Data.Version (showVersion)
import Development.GitRev (gitBranch, gitHash)
import Options.Applicative
import Paths_s_java (version)
import System.FilePath.Glob (glob)
import System.IO
import Text.S
import Text.S.Java (jparser)

data Option = Option Bool Bool String

opts'parser :: ParserInfo Option
opts'parser = info (helper <*> ver <*> options) mempty
 where
  ver =
    (infoOption ver'str . mconcat)
      [long "version", short 'V', help "Print version of the program"]
  ver'str =
    unwords
      ["s-java", showVersion version, "(" ++ $(gitBranch) ++ ")", $(gitHash)]
  options =
    Option
      <$> (switch . mconcat)
        [ long "verbose"
        , short 'v'
        , help "Increase verbosity"
        ]
      <*> (switch . mconcat)
        [ long "test"
        , short 't'
        , help "Test parsing Java code"
        ]
      <*> (strArgument . mconcat)
        [ metavar "FILE"
        , help "File or directory of code"
        ]

main :: IO ()
main = customExecParser (prefs showHelpOnError) opts'parser >>= project

project :: Option -> IO ()
project (Option verbose testMode files) = do
  fs <- glob files
  mapM_ (if testMode then test else undefined) fs
 where
  test f = do
    let p = jparser <* eof
    s <- withFile f ReadMode $ hGetContents >=> (evaluate . force)
    let res = parse p (initState f s)
    case res of
      Ok a _ -> if verbose then pp a else putStrLn f
      Err _ -> die f
