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
fff =
  [here|
    if(hashedValue==null) 3;
//       throw new NullPointerException("For Signature Key :["+signatureKey+"] And Text To Hash ["+textToHash+"] the generated HMAC512 Key is null .Please contact Authorize.net for more information");
|]

f :: String
f =
  [here|
switch (day) {
    case MONDAY, FRIDAY, SUNDAY:
        System.out.println("End of the week!");
        break;

    case 'A':
        System.out.println("Second day of the week!");
        break;

    default:
        System.out.println("Midweek");
        break;
}
|]
