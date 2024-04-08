module Code.Smeter.Internal where

import qualified Data.Map as M

type Lvalue = String

data Violation = Violation
  { v'level :: !(Int, Int) -- (cur, orig)
  , v'loc :: !(Int, Int) -- (line, col)
  , v'var :: !String -- variable
  , v'path :: !String -- scope path
  }

data Scope = Scope
  { s'path :: !String
  , s'level :: !Int
  , s'lmap :: M.Map Lvalue Scope
  , s'gmap :: M.Map Lvalue Scope
  , s'vio :: [Violation]
  }
