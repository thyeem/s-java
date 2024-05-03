{-# LANGUAGE QuasiQuotes #-}

module Code.SS.Internal where

import qualified Data.Map as M
import Data.String.Here

-- type Lvalue = String

-- data Violation = Violation
-- { v'level :: !(Int, Int) -- (cur, orig)
-- , v'loc :: !(Int, Int) -- (line, col)
-- , v'var :: !Lvalue -- variable
-- , v'path :: !String -- scope path
-- }

-- data Scope = Scope
-- { s'path :: !String
-- , s'level :: !Int
-- , s'lmap :: M.Map Lvalue Scope
-- , s'gmap :: M.Map Lvalue Scope
-- , s'vio :: [Violation]
-- }

ff :: String
ff =
  [here|
    protected void encode(ChannelHandlerContext ctx, ControlCommand controlCommand, ByteBuf out) throws Exception {
        byte[] bytes = jsonMapper.writerFor(ControlCommand.class).writeValueAsBytes(controlCommand);
        out.writeBytes(bytes);
    }
|]

fff :: String
fff = "jsonMapper.writerFor(ControlCommand.class).writeValueAsBytes(controlCommand)"

f :: String
f =
  [here|
    switch (input) {
    case 'A', 'B', 'C' -> System.out.println("Letters A, B, or C");
    case 'D' -> System.out.println("Letter D");
    default -> System.out.println("Other letter");
}
|]
