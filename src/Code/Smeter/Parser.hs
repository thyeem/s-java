{-# LANGUAGE QuasiQuotes #-}

module Code.Smeter.Parser where

import Code.Smeter.Internal
import Data.Functor (($>))
import Data.List (intercalate)
import Data.String.Here
import Text.S
  ( Operator (..)
  , Pretty (..)
  , S (..)
  , Stream
  , alpha
  , alphaNum
  , angles'
  , anycharBut
  , anystring
  , char
  , charLit'
  , choice
  , expr
  , float
  , identifier'
  , integer
  , javaDef
  , label
  , many
  , option
  , parens'
  , sepBy
  , some
  , space
  , squares
  , stringLit'
  , symbol'
  , token'
  , void
  , (<?>)
  , (<|>)
  )

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
  _ <- many (modifier <|> generic) -- modifier
  _ <- typedef <|> typ -- type or type-keyword
  i <- iden -- varname
  o <- option [] args'decl
  _ <- many (alpha <|> char ',' <|> space) -- till the first occurrences of '{'
  _ <- symbol "{"
  pure (i, o)

footer :: Stream s => S s ()
footer = void . token $ symbol "}" <?> "closing }"

-- | modifier
modifier :: Stream s => S s String
modifier = label "modifier" . choice $ symbol <$> ["protected", "private", "public", "static", "final"]

-- | type definition keywords
typedef :: Stream s => S s String
typedef = label "typedef keyword" . choice $ symbol <$> ["class", "interface"]

--------------------------------------------------------------------------------

token :: Stream s => S s a -> S s a
token = token' javaDef

angles :: Stream s => S s a -> S s a
angles = angles' javaDef

parens :: Stream s => S s a -> S s a
parens = parens' javaDef

symbol :: Stream s => String -> S s String
symbol = symbol' javaDef

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
iden = identifier' javaDef <?> "identifier"

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
iden'decl = iden <* option mempty (some $ symbol "[]")

-- | identifier for object
iden'obj :: Stream s => S s String
iden'obj = token $ do
  c <- alpha <|> char '_'
  o <- many (alphaNum <|> char '_')
  pure $ c : o

-- | parse a list of L-value from arguments in declaration
--
-- >>> ta args'decl "(final T in, U out, int[][] matrix, String... str)"
-- ["in","out","matrix","str"]
args'decl :: Stream s => S s [String]
args'decl = token $ parens (sepBy (symbol ",") arg)
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
  | Index Jexp [Jexp] -- array access
  | InstOf String Jexp -- instanceOf
  | Cast String Jexp -- type casting
  | New String [Jexp] -- new object
  | Call Jexp [Jexp] -- method invocation
  | Lambda String -- lambda expression
  | Prefix String Jexp -- op: unary prefix
  | Postfix String Jexp -- op: unary postfix
  | Infix String Jexp Jexp -- op: binary infix
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
  null = symbol "null" $> Null
  bool = Bool <$> (symbol "true" <|> symbol "false")
  int = token $ Int <$> integer <* option mempty (symbol "L" <|> symbol "l")
  flt = token $ Float <$> float <* option mempty (symbol "F" <|> symbol "f")
  chr = token $ Char <$> charLit' javaDef
  str = token $ Str <$> stringLit' javaDef

-- | Instanceof expression
expr'iof :: Stream s => S s Jexp
expr'iof = token $ do
  i <- idenpath
  _ <- symbol "instanceof"
  o <- iden'obj
  pure $ InstOf o (Iden i)

-- expr'iof = InstOf <$> iden <* symbol "instanceof" <* iden'obj

-- | Type cast expression
expr'cast :: Stream s => S s Jexp
expr'cast = token $ do
  t <- parens typ
  Cast t . Iden <$> idenpath

-- | Array access expression
expr'idx :: Stream s => S s Jexp
expr'idx = token $ do
  i <- idenpath
  x <- some (squares jexp)
  pure $ Index (Iden i) x

-- Index <$> (iden <* some (squares jexp))

-- | Variable expression
expr'iden :: Stream s => S s Jexp
expr'iden = Iden <$> idenpath

-- | Object creation expression
expr'new :: Stream s => S s Jexp
expr'new = token $ do
  _ <- symbol "new"
  o <- iden'obj
  g <- option mempty generic
  New (o ++ g) <$> args'expr

-- | Lambda expression
expr'lam :: Stream s => S s Jexp
expr'lam = token $ do
  a <- option [] args'decl
  _ <- token $ symbol "->"
  e <- jexp
  pure . Lambda $ concat ["(", intercalate ", " (show <$> a), ") -> ", show e]

-- | Method invocation expression
expr'call :: Stream s => S s Jexp
expr'call = token $ do
  i <- idenpath
  Call (Iden i) <$> args'expr

-- | parse a list of L-value from args in a @call@
--
-- >>> ta (iden *> args'expr) "fn(a, b, c)"
-- [Iden "a",Iden "b",Iden "c"]
args'expr :: Stream s => S s [Jexp]
args'expr = token $ parens (sepBy (symbol ",") jexp)

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
    , [infix' ">", infix' "<", infix' ">=", infix' "<="]
    , [infix' "^", infix' "&", infix' "|"]
    , [infix' "<<", infix' ">>", infix' ">>>"]
    , [infix' "==", infix' "!="]
    , [infix' "&&", infix' "||"]
    ]

infix' :: Stream s => String -> Operator s Jexp
infix' sym = InfixL $ symbol sym $> Infix sym

prefix :: Stream s => String -> Operator s Jexp
prefix sym = PrefixU $ symbol sym $> Prefix sym

postfix :: Stream s => String -> Operator s Jexp
postfix sym = PostfixU $ symbol sym $> Postfix sym

-- | Assignment statement
assign :: Stream s => S s String
assign = S $ \s fOk fErr ->
  let fOk' a = fOk (a ++ "francis")
   in unS anystring s fOk' fErr

stmt :: Stream s => S s String
stmt = undefined
