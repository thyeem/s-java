module Code.SS.Parser where

import Code.SS.Internal
import Data.Functor (($>))
import Data.List (intercalate)
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
  , braces'
  , char
  , charLit'
  , choice
  , expr
  , floatB
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

-- $setup
-- >>> import Text.S

--------------------------------------------------------------------------------

token :: Stream s => S s a -> S s a
token = token' javaDef

angles :: Stream s => S s a -> S s a
angles = angles' javaDef

parens :: Stream s => S s a -> S s a
parens = parens' javaDef

braces :: Stream s => S s a -> S s a
braces = braces' javaDef

symbol :: Stream s => String -> S s String
symbol = symbol' javaDef

-- | modifier
modifier :: Stream s => S s String
modifier = choice $ symbol <$> ["protected", "private", "public", "static", "final"]

-- | type definition keywords
typedef :: Stream s => S s String
typedef = choice $ symbol <$> ["class", "interface"]

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
idenpath =
  iden >>= \i ->
    intercalate "." . (i :) <$> many (symbol "." *> iden)

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
  | Lambda [Jexp] Jstmt -- lambda expression
  | Cond Jexp Jexp Jexp -- ternary expression
  | Prefix String Jexp -- prefix unary operator
  | Postfix String Jexp -- postfix unary operator
  | Infix String Jexp Jexp -- binary infix operator
  deriving (Show)

instance Pretty Jexp

-- | Expressions in Java
jexp :: Stream s => S s Jexp
jexp = choice [expr'cond, expr'op, factor]

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
  flt = token $ Float <$> floatB <* option mempty (symbol "F" <|> symbol "f")
  chr = token $ Char <$> charLit' javaDef
  str = token $ Str <$> stringLit' javaDef

-- | Instanceof expression
--
-- >>> ta expr'iof "name instanceof String"
-- InstOf "String" (Iden "name")
expr'iof :: Stream s => S s Jexp
expr'iof = token $ do
  i <- idenpath
  _ <- symbol "instanceof"
  o <- iden'obj
  pure $ InstOf o (Iden i)

-- | Type cast expression
--
-- >>> ta expr'cast "(int) floating"
-- Cast "int" (Iden "floating")
expr'cast :: Stream s => S s Jexp
expr'cast = token $ do
  t <- parens typ
  Cast t . Iden <$> idenpath

-- | Array access expression
--
-- >>> ta expr'idx "arr[i][0]"
-- Index (Iden "arr") [Iden "i",Int 0]
expr'idx :: Stream s => S s Jexp
expr'idx = token $ do
  i <- idenpath
  x <- some (squares jexp)
  pure $ Index (Iden i) x

-- | Variable expression
expr'iden :: Stream s => S s Jexp
expr'iden = Iden <$> idenpath

-- | Object creation expression
--
-- >>> ta expr'new "new Object(\"obj\", 'Q', 12.34)"
-- New "Object" [Str "obj",Char 'Q',Float 12.34]
--
-- >>> ta expr'new "new int[]{1, 2, 3}"
-- New "int[]" [Int 1,Int 2,Int 3]
expr'new :: Stream s => S s Jexp
expr'new = token $ do
  _ <- symbol "new"
  t <- typ
  New t <$> (args'expr <|> args'arr)

-- | Lambda expression
-- Lambda in Java consists of expr-statement and block-statement
expr'lam :: Stream s => S s Jexp
expr'lam = token $ do
  a <- args'decl True
  _ <- symbol "->"
  Lambda a <$> (stmt'expr <|> (Scope mempty a <$> stmt'block))

-- | Method invocation expression
--
-- >>> ta expr'call "Integer.valueOf(2)"
-- Call (Iden "Integer.valueOf") [Int 2]
expr'call :: Stream s => S s Jexp
expr'call = token $ do
  i <- idenpath
  Call (Iden i) <$> args'expr

-- | Parse L-values from a list of arguments in declaration
-- If 'optType' is set, bare-type variables are admitted.
--
-- >>> ta (args'decl False) "(final U out, int[][] matrix, String... str)"
-- [Iden "out",Iden "matrix",Iden "str"]
--
-- >>> ta (args'decl True) "(out, matrix, str)"
-- [Iden "out",Iden "matrix",Iden "str"]
args'decl :: Stream s => Bool -> S s [Jexp]
args'decl optType = token $ parens (sepBy (symbol ",") arg)
 where
  pair = typ *> var
  var = Iden <$> iden'decl
  arg =
    option mempty anno
      *> option mempty (symbol "final")
      *> (if optType then pair <|> var else typ *> var)

-- | Parse L-values from a list of 'Jexp' arguments
--
-- >>> ta args'expr "(a,b,c)"
-- [Iden "a",Iden "b",Iden "c"]
args'expr :: Stream s => S s [Jexp]
args'expr = token $ parens (sepBy (symbol ",") jexp)

-- | Parse L-values from array initialization expression
--
-- >>> ta args'arr "{1,2,3}"
-- [Int 1,Int 2,Int 3]
args'arr :: Stream s => S s [Jexp]
args'arr = token $ braces (sepBy (symbol ",") jexp)

-- | Ternary expression
--
-- >>> ta expr'cond "(10 > 5) ? 1 : 0"
-- Cond (Infix ">" (Int 10) (Int 5)) (Int 1) (Int 0)
expr'cond :: Stream s => S s Jexp
expr'cond = token $ do
  c <- atom
  _ <- symbol "?"
  x <- atom
  _ <- symbol ":"
  Cond c x <$> atom
 where
  atom = choice [parens expr'cond, expr'op, factor]

-- | Operator-related expression
--
-- >>> ta expr'op "fn(5) % 2 != 0"
-- Infix "!=" (Infix "%" (Call (Iden "fn") [Int 5]) (Int 2)) (Int 0)
--
-- >>> ta expr'op "(5 > 3) && (8 > 5)"
-- Infix "&&" (Infix ">" (Int 5) (Int 3)) (Infix ">" (Int 8) (Int 5))
expr'op :: Stream s => S s Jexp
expr'op = expr atom table
 where
  atom = choice [factor, parens expr'op, parens expr'cond]
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

data Jstmt
  = Scope String [Jexp] [Jstmt] -- new scope
  | Assign Jexp Jexp -- assignment
  | Expr Jexp -- expression statement
  deriving (Show)

instance Pretty Jstmt

-- | Java statement parser
jstmt :: Stream s => S s Jstmt
jstmt = choice [stmt'expr]

-- | Java [statement] parser
jstmts :: Stream s => S s [Jstmt]
jstmts =
  sepBy (symbol ";") jstmt >>= \s ->
    if length s == 1
      then option mempty (symbol ";") $> s
      else pure s

-- | New scope statment
--
-- ta stmt'scope "class Counter { coffee.drip(\"ethiopia\"); }"
-- ("Counter",[])
--
-- ta stmt'scope "public class ConcurrencyProblemDemo {"
-- ("ConcurrencyProblemDemo",[])
--
-- ta stmt'scope "static void main(String[] args) throws InterruptedException {"
-- ("main",["args"])
--
-- ta stmt'scope "public static void gcd(int n1, int n2) {"
-- ("gcd",["n1","n2"])
--
-- ta stmt'scope "public <T, U> void fn(final T in, U out, int[][] matrix, String... str) {"
-- ("fn",["in","out","matrix","str"])
--
-- ta stmt'scope "public static String getHMACSHA512(String signatureKey, String textToHash)throws Exception{"
-- ("getHMACSHA512",["signatureKey","textToHash"])
--
-- ta stmt'scope "public static String calculateHMAC(byte signatureKey[], String textToHash) {"
-- ("calculateHMAC",["signatureKey","textToHash"])
--
-- ta stmt'scope "private static String toHexString(byte[] bytes) { 3 + 5; }"
-- Scope "toHexString" [Iden "bytes"] [Expr (Infix "+" (Int 3) (Int 5))]
--
-- ta stmt'scope "public static byte[] hexStringToByteArray(String str) {}"
-- Scope "hexStringToByteArray" [Iden "str"] []
stmt'scope :: Stream s => S s Jstmt
stmt'scope = do
  _ <- option mempty (many modifier) -- modifiers
  _ <- option mempty generic -- generic
  n <- (typedef *> iden'obj) <|> (typ *> iden) -- name of scope
  a <- args'decl False -- type-iden pairs in argument declaration
  _ <- many (alpha <|> char ',' <|> space) -- till the first occurrences of '{'
  Scope n a <$> stmt'block

-- | Expression statement
stmt'expr :: Stream s => S s Jstmt
stmt'expr = Expr <$> jexp <* option mempty (symbol ";")

-- | Block statement
stmt'block :: Stream s => S s [Jstmt]
stmt'block = braces jstmts <* option mempty (symbol ";")

-- | Assignment statement
stmt'let :: Stream s => S s String
stmt'let = S $ \s fOk fErr ->
  let fOk' a = fOk (a ++ "francis")
   in unS anystring s fOk' fErr
