{-# LANGUAGE QuasiQuotes #-}

module Code.Smeter (module Code.Smeter) where

import Data.Functor (($>))
import Data.String.Here
import Text.S

src :: String
src =
  [here|
class Counter {
    private int count = 0;

    public void increment() {
        count = count + 1;
    }

    public int getCount() {
        return count;
    }
}

public class ConcurrencyProblemDemo {
    public static void main(String[] args) throws InterruptedException {
        final Counter counter = new Counter();

        Thread thread1 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                counter.increment();
            }
        });

        Thread thread2 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                counter.increment();
            }
        });

        thread1.start();
        thread2.start();

        thread1.join();
        thread2.join();

        System.out.println("Expected count is 2000");
        System.out.println("Actual count is " + counter.getCount());
    }
}
|]

type Lvalue = String

token :: Stream s => S s a -> S s a
token p = p <* leap

leap :: Stream s => S s ()
leap = skip' javaDef

-- | header of a new scope
--
-- >>> ta header "class Counter {"
-- ("Counter",[])
--
-- >>> ta header "public class ConcurrencyProblemDemo {"
-- ("ConcurrencyProblemDemo",[])
--
-- >>> ta header "    public static void main(String[] args) throws InterruptedException {"
-- ("main",["args"])
--
-- >>> ta header "    public static void gcd(int n1, int n2) {"
-- ("gcd",["n1","n2"])
--
-- >>> ta header "public <T, U> void fn(final T in, U out, int[][] matrix, String... str) {"
-- ("fn",["in","out","matrix","str"])
--
-- >>> ta header "public static String getHMACSHA512(String signatureKey, String textToHash)throws Exception{"
-- ("getHMACSHA512",["signatureKey","textToHash"])
--
-- >>> ta header "public static String calculateHMAC(byte signatureKey[], String textToHash) {"
-- ("calculateHMAC",["signatureKey","textToHash"])
--
-- >>> ta header "private static String toHexString(byte[] bytes) {"
-- ("toHexString",["bytes"])
--
-- >>> ta header "public static byte[] hexStringToByteArray(String str) {"
-- ("hexStringToByteArray",["str"])
header :: Stream s => S s (Lvalue, [Lvalue])
header = do
  leap -- skip the unnecesary
  _ <- many (modifier <|> generic) -- modifier
  _ <- typedef <|> typ -- type or type-keyword
  i <- iden -- varname
  o <- option [] args -- (arg1, arg2, ..)
  _ <- many (alpha <|> char ',' <|> space) -- till the first occurrences of '{'
  _ <- string "{"
  pure (i, o)

footer :: Stream s => S s ()
footer = void . token $ symbol "}" <?> "closing }"

-- | modifier
modifier :: Stream s => S s String
modifier = label "modifier" . token . choice $ symbol <$> ["protected", "private", "public", "static", "final"]

-- | type definition keywords
typedef :: Stream s => S s String
typedef = label "typedef keyword" . token . choice $ symbol <$> ["class", "interface"]

-- | type
-- >>> ta typ "int[][] c0ffee"
-- "int[][]"
--
-- >>> ta typ "Drinks<T> c0ffee"
-- "Drinks<T>"
--
-- >>> ta typ "String... c0ffee"
-- "String..."
typ :: Stream s => S s String
typ =
  label "type" . token $
    symbol "void"
      <|> do
        i <- var -- object name
        g <- option mempty generic -- diamond
        l <- option mempty (some $ symbol "[]") -- ndarry
        v <- option mempty (symbol "...") -- varargs
        pure $ concat [i, g, concat l, v]

-- | generic diamond
--
-- >>> ta generic "<T,U> c0ffee[]"
-- "<T,U>"
generic :: Stream s => S s String
generic = label "generic" . token $ do
  o <- angles (many $ anycharBut '>')
  pure $ "<" ++ o ++ ">"

-- | annotations
--
-- >>> ta anno "@atCafe string c0ffee"
-- "@atCafe"
--
-- >>> ta anno "@SuppressWarnings(bool=false) public"
-- "@SuppressWarnings(bool=false)"
anno :: Stream s => S s String
anno = label "annotation" . token $ do
  c <- char '@' -- sigil
  n <- var -- anno varname
  o <-
    option
      mempty
      ( do
          p <- parens (many $ anycharBut ')')
          pure $ "(" ++ p ++ ")"
      ) -- optional (...)
  pure $ c : n ++ o

-- | identifier
--
-- >>> ta iden "_c0ffee"
-- "_c0ffee"
--
-- >>> ta iden "c0ffee[][]"
-- "c0ffee"
iden :: Stream s => S s String
iden = identifier javaDef <* option mempty (some $ symbol "[]") <?> "identifier"

-- | object/variable name NOT in the keyword set
var :: Stream s => S s String
var = label "object name" . token $ do
  c <- alpha <|> char '_'
  o <- many (alphaNum <|> char '_')
  pure $ c : o

-- | parse a list of L-value from an argument declaration
--
-- >>> ta args "(final T in, U out, int[][] matrix, String... str)"
-- ["in","out","matrix","str"]
args :: Stream s => S s [String]
args = label "[argument]" . token $ parens (sepBy (symbol ",") arg)

-- | parse a L-value from
--
-- >>> ta arg "@atCafe string c0ffee"
-- "c0ffee"
--
-- >>> ta arg "int[][] c0ffee"
-- "c0ffee"
--
-- >>> ta arg "final string... c0ffee"
-- "c0ffee"
--
-- >>> ta arg "Drinks<T> c0ffee"
-- "c0ffee"
arg :: Stream s => S s String
arg = label "argument" . token $ do
  leap -- skip the unnecesary
  _ <- option mempty anno -- annotation
  _ <- option mempty (symbol "final") -- final keyword
  _ <- typ -- type
  iden -- varname

-- | Expressions in Java
jexp :: Stream s => S s String
jexp = expr'op <|> factor

factor :: Stream s => S s String
factor = choice [expr'new, expr'cast, expr'iof, expr'idx, expr'var, expr'prim]

-- | Primitive literals
expr'prim :: Stream s => S s String
expr'prim = choice [bool, flt, int, chr, str]
 where
  bool = token $ string "true" <|> string "false"
  int = token $ show <$> integer <* option mempty (string "L" <|> string "l")
  flt = token $ show <$> float <* option mempty (string "F" <|> string "f")
  chr = token $ pure <$> charLit' javaDef
  str = token $ stringLit' javaDef

-- | Variable expression
expr'var :: Stream s => S s String
expr'var = iden

-- | Instanceof expression
expr'iof :: Stream s => S s String
expr'iof = iden <* symbol "instanceof" <* var

-- | Type cast expression
expr'cast :: Stream s => S s String
expr'cast = parens typ *> iden

-- | Array access expression
expr'idx :: Stream s => S s String
expr'idx = (parens jexp <|> expr'var) <* some (squares jexp)

-- | Object creation expression
expr'new :: Stream s => S s String
expr'new = token $ symbol "new" *> var <* (option mempty generic >> arguments)
 where
  arguments = parens (sepBy (symbol ",") jexp)

expr'op :: Stream s => S s String
expr'op = expr atom table
 where
  bop sym = InfixL $ strip (symbol sym) $> (\x y -> unwords [x, sym, y])
  pop sym = PrefixU $ strip (symbol sym) $> (\x -> unwords [sym, x])
  qop sym = PostfixU $ strip (symbol sym) $> (\x -> unwords [x, sym])
  atom = factor <|> parens expr'op
  table =
    [ [pop "-", pop "+"]
    , [pop "++", pop "--", qop "++", qop "--"]
    , [bop "^"]
    , [bop "*", bop "/"]
    , [bop "+", bop "-"]
    ]
