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
import java.util.Arrays;
import java.util.List;

public class LambdaExample {
    public static void main(String[] args) {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);

        // Using lambda with a single statement (without braces)
        numbers.forEach(n -> System.out.println(n));

        // Alternatively, the same lambda with braces (for multiple statements or clarity)
        numbers.forEach(n -> {
            System.out.println(n);
            // Add more statements here if needed
        });
    }
}
|]
