{-# LANGUAGE QuasiQuotes #-}

module Code.Smeter.Parser where

import Code.Smeter.Internal
import Data.Functor (($>))
import Data.List (intercalate)
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
  o <- option [] args'decl
  _ <- many (alpha <|> char ',' <|> space) -- till the first occurrences of '{'
  _ <- string "{"
  pure (i, o)

footer :: Stream s => S s ()
footer = void . token $ symbol "}" <?> "closing }"

--------------------------------------------------------------------------------

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
        i <- iden'obj -- type name
        g <- option mempty generic -- diamond
        l <- option mempty (some $ symbol "[]") -- ndarry
        v <- option mempty (symbol "...") -- varargs
        pure $ concat [i, g, concat l, v]

-- | generic diamond
--
-- >>> ta generic "<T,U> c0ffee[]"
-- "<T,U>"
generic :: Stream s => S s String
generic =
  label "generic" . token $
    angles (many $ anycharBut '>') >>= \p -> pure $ "<" ++ p ++ ">"

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
  n <- iden'obj -- anno varname
  o <- -- optional (...)
    option
      mempty
      (parens (many $ anycharBut ')') >>= \p -> pure $ "(" ++ p ++ ")")
  pure $ c : n ++ o

-- | identifier
--
-- >>> ta iden "_c0ffee"
-- "_c0ffee"
iden :: Stream s => S s String
iden = identifier javaDef <?> "identifier"

-- | identifier with relative path
--
-- >>> ta idenpath "coffee.ethiopia.handrip"
-- "coffee.ethiopia.handrip"
idenpath :: Stream s => S s String
idenpath = iden >>= \i -> intercalate "." . (i :) <$> many (symbol "." *> iden)

-- | identifier for declaration
--
-- >>> ta iden'decl "_c0ffee[][]"
-- "_c0ffee"
iden'decl :: Stream s => S s String
iden'decl = identifier javaDef <* option mempty (some $ symbol "[]") <?> "identifier"

-- | identifier for object
iden'obj :: Stream s => S s String
iden'obj = label "object name" . token $ do
  c <- alpha <|> char '_'
  o <- many (alphaNum <|> char '_')
  pure $ c : o

-- | parse a list of L-value from arguments in declaration
--
-- >>> ta args'decl "(final T in, U out, int[][] matrix, String... str)"
-- ["in","out","matrix","str"]
args'decl :: Stream s => S s [String]
args'decl = label "[type-iden]" . token $ parens (sepBy (symbol ",") arg)
 where
  arg = option mempty anno *> option mempty (symbol "final") *> typ *> iden'decl

data Jexp
  = Null -- primitive null
  | Bool String -- primitive true/false
  | Int Integer -- primitive integer
  | Float Double -- primitive float
  | Char Char -- char literal
  | Str String -- string literal
  | Iden String -- iden-path
  | Prefix String Jexp -- unary prefix
  | Postfix String Jexp -- unary postfix
  | Infix String Jexp Jexp -- binary infix
  | Index String -- array access
  | InstOf String -- instanceOf
  | Cast String -- type casting
  | New String -- new object
  | Lambda String -- lambda expression
  | Call String -- method invocation
  deriving (Show)

instance Pretty Jexp

-- | Expressions in Java
jexp :: Stream s => S s Jexp
jexp = expr'op <|> factor

-- | Unit expressions with the highest priority
factor :: Stream s => S s Jexp
factor = parens e <|> e
 where
  e =
    choice
      [ expr'call
      , expr'lam
      , expr'new
      , expr'cast
      , expr'iof
      , expr'idx
      , expr'iden
      , expr'prim
      ]

-- | Primitive literals
expr'prim :: Stream s => S s Jexp
expr'prim = choice [flt, int, chr, str, bool, null]
 where
  null = token $ string "null" $> Null
  bool = token $ Bool <$> (string "true" <|> string "false")
  int = token $ Int <$> integer <* option mempty (string "L" <|> string "l")
  flt = token $ Float <$> float <* option mempty (string "F" <|> string "f")
  chr = token $ Char <$> charLit' javaDef
  str = token $ Str <$> stringLit' javaDef

-- | Instanceof expression
expr'iof :: Stream s => S s Jexp
expr'iof = InstOf <$> iden <* symbol "instanceof" <* iden'obj

-- | Type cast expression
expr'cast :: Stream s => S s Jexp
expr'cast = Cast <$> (parens typ *> iden)

-- | Array access expression
expr'idx :: Stream s => S s Jexp
expr'idx = Index <$> (iden <* some (squares jexp))

-- | Object creation expression
expr'new :: Stream s => S s Jexp
expr'new =
  token $
    New <$> (symbol "new" *> iden'obj <* (option mempty generic >> arguments))
 where
  arguments = parens (sepBy (symbol ",") jexp)

-- | Variable expression
expr'iden :: Stream s => S s Jexp
expr'iden = Iden <$> idenpath

-- | Lambda expression
expr'lam :: Stream s => S s Jexp
expr'lam = token $ do
  a <- option [] args'decl
  _ <- token $ symbol "->"
  e <- jexp
  pure $ Lambda $ concat ["(", intercalate ", " (show <$> a), ") -> ", show e]

-- | Method invocation expression
expr'call :: Stream s => S s Jexp
expr'call = token $ do
  i <- idenpath
  a <- args'expr
  pure . Call $ concat [i, "(", intercalate ", " (show <$> a), ")"]

-- | parse a list of L-value from args in a @call@
--
-- >>> ta (iden *> args'expr) "fn(a, b, c)"
-- [Iden "a",Iden "b",Iden "c"]
args'expr :: Stream s => S s [Jexp]
args'expr = label "[Jexp]" . token $ parens (sepBy (symbol ",") jexp)

-- | Operator-related expression
expr'op :: Stream s => S s Jexp
expr'op = expr atom table
 where
  atom = factor <|> parens expr'op
  table =
    [ [prefix "-", prefix "+", prefix "!"]
    , [prefix "++", prefix "--", postfix "++", postfix "--"]
    , [infix' "*", infix' "/", infix' "%"]
    , [infix' "+", infix' "-"]
    , [infix' "%%", infix' "||"]
    , [infix' ">", infix' "<", infix' ">=", infix' "<="]
    , [infix' "^", infix' "&", infix' "|"]
    , [infix' "<<", infix' ">>", infix' ">>>"]
    , [infix' "==", infix' "!="]
    ]

infix' :: Stream s => String -> Operator s Jexp
infix' sym = InfixL $ strip (symbol sym) $> Infix sym

prefix :: Stream s => String -> Operator s Jexp
prefix sym = PrefixU $ strip (symbol sym) $> Prefix sym

postfix :: Stream s => String -> Operator s Jexp
postfix sym = PostfixU $ strip (symbol sym) $> Postfix sym

-- | Assignment statement
assign :: Stream s => S s String
assign = S $ \s fOk fErr ->
  let fOk' a = fOk (a ++ "francis")
   in unS anystring s fOk' fErr
