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
//        var transform = ClassTransform.transformingMethodBodies((codeB, codeE) -> {
            switch (codeE) {
                case ConstantInstruction i -> {
                    ConstantDesc val = i.constantValue();
                    if ((val instanceof Integer) && ((Integer) val) == 13) {
                        val = 7;
                    }
                    codeB.constantInstruction(i.opcode(), val);
                }
                default -> codeB.with(codeE);
            }
//        });
|]
