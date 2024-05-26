{-# LANGUAGE QuasiQuotes #-}

module Code.SS.Internal where

import Code.SS.Parser (Identifier)
import qualified Data.Map as M
import Data.String.Here

-- | Scope tracker
data Tracker = Tracker
  { t'path :: !String
  , t'level :: !Int
  , t'lamp :: M.Map String Tracker -- local map: key(iden-string)
  , t'gmap :: M.Map String Tracker -- global map: key(iden-string)
  , t'violations :: [Violation] -- List of violations
  }
  deriving (Show)

-- | Violation
data Violation
  = Violation (String, Identifier) (String, Identifier) -- pair of (path, iden)
  deriving (Show)

ff :: String
ff =
  [here|
        for (int i = 0; i < v.size(); i++) {
            Font f = v.get(i);
            int sw = getFontMetrics(f).stringWidth(f.getName());
            g2.setFont(f);
            g2.drawString(f.getName(), (w / 2 - sw / 2), yy = strH);
        }

|]
