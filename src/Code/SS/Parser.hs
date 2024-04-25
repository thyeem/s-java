{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Code.SS.Parser where

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
  , between
  , braces'
  , char
  , charLit'
  , choice
  , expr
  , floatB
  , identifier'
  , integer
  , javaDef
  , jump'
  , many
  , option
  , parens'
  , sepBy
  , sepBy1
  , some
  , space
  , squares'
  , stringLit'
  , symbol'
  , token'
  , (<?>)
  , (<|>)
  )

-- $setup
-- >>> import Text.S

-- | Skips unnecesary whitespaces and comments
jump :: Stream s => S s ()
jump = jump' javaDef

-- | Parser for Java token
token :: Stream s => S s a -> S s a
token = token' javaDef

-- | Parser for < .. >
angles :: Stream s => S s a -> S s a
angles = angles' javaDef

-- | Parser for ( .. )
parens :: Stream s => S s a -> S s a
parens = parens' javaDef

-- | Parser for { .. }
braces :: Stream s => S s a -> S s a
braces = braces' javaDef

-- | Parser for [ .. ]
squares :: Stream s => S s a -> S s a
squares = squares' javaDef

-- | Parser for given strings
symbol :: Stream s => String -> S s String
symbol = symbol' javaDef

-- | modifier
modifier :: Stream s => S s String
modifier =
  choice $
    symbol <$> ["protected", "private", "public", "abstract", "static", "final"]

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
  token $
    symbol "void" <|> do
      i <- iden'typ -- type name
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
  token $
    angles (many $ anycharBut '>') >>= \p -> pure $ "<" ++ p ++ ">"

-- | annotations
--
-- >>> ta anno "@atCafe string c0ffee"
-- "@atCafe"
--
-- >>> ta anno "@SuppressWarnings(bool=false) public"
-- "@SuppressWarnings(bool=false)"
anno :: Stream s => S s String
anno = token $ do
  c <- char '@' -- sigil
  n <- iden'typ -- anno varname
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

-- | identifier for type/object
--
-- >>> ta iden'typ "int"
-- "int"
iden'typ :: Stream s => S s String
iden'typ = token $ do
  c <- alpha <|> char '_'
  o <- many (alphaNum <|> char '_')
  pure $ c : o

-- | identifier for declaration
--
-- >>> ta iden'decl "_c0ffee[][]"
-- "_c0ffee"
iden'decl :: Stream s => S s String
iden'decl = iden <* option mempty (some $ symbol "[]")

-- | Definition of Java expression
data Jexp
  = Null -- primitive null
  | Bool String -- primitive true/false
  | Int Integer -- primitive integer
  | Float Double -- primitive float
  | Char Char -- char literal
  | Str String -- string literal
  | Iden String -- identifier
  | Chain [Jexp] -- field/method/reference chain
  | Array [Jexp] -- array initialization
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
  | This -- 'this' special keyword
  | O -- nil expression
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
      [ expr'chain -- call/this/iden
      , expr'lam -- lambda
      , expr'new -- new Object
      , expr'cast -- (type) expr
      , expr'iof -- instanceof
      , expr'arr -- array init {1,2,3}
      , expr'idx -- num[i][j]
      , expr'prim -- primitive
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
  i <- expr'chain
  _ <- symbol "instanceof"
  o <- iden'typ
  pure $ InstOf o i

-- | Type cast expression
--
-- >>> ta expr'cast "(int) floating"
-- Cast "int" (Iden "floating")
expr'cast :: Stream s => S s Jexp
expr'cast = token $ parens typ >>= \t -> Cast t <$> jexp

-- | Array initialization expression
expr'arr :: Stream s => S s Jexp
expr'arr = Array <$> args'arr

-- | Array access expression
--
-- >>> ta expr'idx "arr[i][0]"
-- Index (Iden "arr") [Iden "i",Int 0]
expr'idx :: Stream s => S s Jexp
expr'idx = token $ expr'chain >>= \i -> Index i <$> some (squares jexp)

-- | Object creation expression
--
-- >>> ta expr'new "new Object(\"obj\", 'Q', 12.34)"
-- New "Object" [Str "obj",Char 'Q',Float 12.34]
--
-- >>> ta expr'new "new int[]{1, 2, 3}"
-- New "int[]" [Int 1,Int 2,Int 3]
--
-- >>> ta expr'new "new byte[len / 2]"
-- New "byte" [Infix "/" (Iden "len") (Int 2)]
expr'new :: Stream s => S s Jexp
expr'new =
  token $
    (symbol "new" *> typ)
      >>= \t -> New t <$> choice [args'expr, args'arr, some (squares jexp)]

-- | Lambda expression
-- Lambda in Java consists of expr-statement and block-statement
expr'lam :: Stream s => S s Jexp
expr'lam = token $ do
  a <- args'decl True <|> (: []) <$> expr'iden -- (args) or value
  _ <- symbol "->"
  Lambda a <$> ((Scope "\\" a <$> block) <|> stmt'expr)

-- | Variable expression
expr'iden :: Stream s => S s Jexp
expr'iden = Iden <$> iden

-- | Keyword 'this'
-- 'this' isn't a 'jexp', but can be thought of as an expression semantically
expr'this :: Stream s => S s Jexp
expr'this = This <$ symbol "this"

-- | Method invocation expression
--
-- >>> ta expr'call "valueOf(2)"
-- Call (Iden "valueOf") [Int 2]
expr'call :: Stream s => S s Jexp
expr'call = token $ expr'iden >>= \i -> Call i <$> args'expr

-- | Field/method chaining
--
-- >>> ta expr'chain "coffee.espresso(25)"
-- Chain [Iden "coffee",Call (Iden "espresso") [Int 25]]
--
-- >>> ta expr'chain "coffee"
-- Iden "coffee"
--
-- >>> ta expr'chain "this.coffee"
-- Chain [This,Iden "coffee"]
expr'chain :: Stream s => S s Jexp
expr'chain = do
  o <-
    sepBy1
      ( symbol "." -- access field/method
          <|> symbol "::" -- method reference
      )
      (choice [expr'call, expr'this, expr'iden])
  if length o == 1 then pure (head o) else pure (Chain o)

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

-- | Definition of Java statement
data Jstmt
  = Package String -- package statement
  | Import String -- import statement
  | Abstract Jexp [Jexp] -- abstract method statement
  | Scope String [Jexp] [Jstmt] -- new scope
  | Assign String Jexp Jexp -- assignment/declaration
  | Return Jexp -- return statement
  | Flow String -- flow control statement
  | Expr Jexp -- expression statement
  | If Jexp Jstmt [Jstmt] -- if-statement
  | Else Jexp Jstmt -- else-if/else block (only valid in if-statement)
  | Switch Jexp [Jstmt] -- switch-statement
  | Case Jexp [Jstmt] -- case clause (only valid in switch-statement)
  | Try Jstmt [Jstmt] -- try-catch-finally statement
  | Catch Jexp Jstmt -- catch block (only valid in try-statement)
  | For Jstmt -- for-statement
  | While Jexp Jstmt -- while-statement
  | Do Jstmt Jexp -- do-while-statement
  | Enum [Jexp] -- enum declaration statement
  | Throw Jexp -- throw statement
  | Sync Jexp Jstmt -- synchronized statement
  deriving (Show)

instance Pretty Jstmt

-- | Java statement parser
jstmt :: Stream s => S s Jstmt
jstmt =
  between
    jump
    jump
    ( choice
        [ stmt'pkg
        , stmt'import
        , stmt'scope
        , stmt'abs
        , stmt'bare
        , stmt'if
        , stmt'switch
        , stmt'try
        , stmt'for
        , stmt'while
        , stmt'sync
        , stmt'enum
        , stmt'throw
        , stmt'ret
        , stmt'assign
        , stmt'flow
        , stmt'expr
        ]
    )

-- | Java [statement] parser
jstmts :: Stream s => S s [Jstmt]
jstmts = do
  a <- many $ jstmt >>= \s -> (if need'st s then s <$ symbol ";" else pure s)
  x <- many jstmt
  if
      | null a -> option mempty (symbol ";") $> x -- single statement
      | null x -> pure a -- well-formed multiple statement
      | otherwise -> symbol ";" $> a ++ x -- malformed multiple statement

-- | Check if the ST (statement terminator or ';') is needed
need'st :: Jstmt -> Bool
need'st = \case
  Package {} -> True
  Import {} -> True
  Abstract {} -> True
  Assign {} -> True
  Return {} -> True
  Throw {} -> True
  Flow {} -> True
  Expr {} -> True
  Do {} -> True -- do-while
  If _ e _ -> case e of
    Expr {} -> True -- single expr in if-body
    Flow {} -> True -- break/continue
    Throw {} -> True -- throw
    Return {} -> True -- return
    _ -> False -- multiple statment cases
  _ -> False -- otherwise

-- | Common block parser
block :: Stream s => S s [Jstmt]
block = braces jstmts <* option mempty (symbol ";")

-- | New scope statment
--
-- >>> ta stmt'scope "public static byte[] hexStringToByteArray(String str) {}"
-- Scope "hexStringToByteArray" [Iden "str"] []
--
-- >>> ta stmt'scope "static void main(String[] args) throws Exception {}"
-- Scope "main" [Iden "args"] []
--
-- >>> ta stmt'scope "public <T, U> void fn(final T in, U out, int[][] matrix, String... str) {}"
-- Scope "fn" [Iden "in",Iden "out",Iden "matrix",Iden "str"] []
--
-- >>> ta stmt'scope "public static void mod(int a, int b) { return a % b; }"
-- Scope "mod" [Iden "a",Iden "b"] [Return (Infix "%" (Iden "a") (Iden "b"))]
--
-- >>> ta stmt'scope "private static String toHexString(byte[] bytes) { 3 + 5; }"
-- Scope "toHexString" [Iden "bytes"] [Expr (Infix "+" (Int 3) (Int 5))]
--
-- >>> ta stmt'scope "class Ethiopia { void drip(Coffee bean) {} }"
-- Scope "Ethiopia" [] [Scope "drip" [Iden "bean"] []]
stmt'scope :: Stream s => S s Jstmt
stmt'scope = do
  let g = option mempty generic
  _ <- option mempty (many anno) -- annotations
  _ <- option mempty (many modifier) -- modifiers
  _ <- g -- generic before Name
  i <-
    choice
      [ typedef *> g *> iden'typ -- class/interface Name
      , typ *> g *> iden -- Type Name
      , iden'typ -- Name
      ]
  _ <- g -- generic after Name
  a <- option [] (args'decl False) -- type-iden pairs in argument declaration
  _ <- many $ choice [alpha, char ',', space] -- ignore throws/extends
  Scope i a <$> block

-- | Abstract method statement
stmt'abs :: Stream s => S s Jstmt
stmt'abs = do
  _ <- option mempty (many anno) -- annotations
  i <-
    symbol "abstract"
      *> typ
      *> option mempty generic
      *> iden'typ
      <* option mempty generic -- Type Name
  a <- args'decl False -- type-iden pairs in argument declaration
  pure $ Abstract (Iden i) a

-- | enum statement
stmt'enum :: Stream s => S s Jstmt
stmt'enum = do
  _ <- option mempty (many anno) -- annotations
  _ <- option mempty (many modifier) -- modifiers
  i <- symbol "enum" *> iden'typ -- enum Name
  Scope i []
    <$> braces
      ( do
          e <- Enum <$> (sepBy (symbol ",") jexp <* option mempty (symbol ";"))
          s <- jstmts
          pure $ e : s
      )

-- | Unnamed block statement
--
-- >>> ta stmt'block "{ coffee.roasted(); }"
-- Scope "" [] [Expr (Chain [Iden "coffee",Call (Iden "roasted") []])]
stmt'block :: Stream s => S s Jstmt
stmt'block = Scope mempty [] <$> block

-- | Bare-block statement
stmt'bare :: Stream s => S s Jstmt
stmt'bare = do
  _ <- option mempty (symbol "static")
  _ <- option mempty (choice $ symbol <$> ["synchronized", "final", "transient"])
  Scope mempty [] <$> block

-- | Assignment statement
--
-- >>> ta stmt'assign "int number"
-- Assign "" (Iden "number") O
--
-- >>> ta stmt'assign "int number = 5"
-- Assign "=" (Iden "number") (Int 5)
stmt'assign :: Stream s => S s Jstmt
stmt'assign = do
  _ <- option mempty (many anno) -- annotations
  _ <- option mempty (many modifier) -- modifiers
  ( choice
      [ expr'iden'decl -- type iden
      , expr'idx -- iden[i][j]
      , expr'iden -- iden
      ]
      >>= \i -> choice ops >>= \op -> Assign op i <$> jexp -- op(=,+=,..) jexp
    )
    <|> (expr'iden'decl >>= \i -> pure (Assign mempty i O)) -- type iden;
 where
  expr'iden'decl = Iden <$> (typ *> iden'decl)
  ops =
    symbol
      <$> ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]

-- | Return statement
--
-- >>> ta stmt'ret "return (10 > 5) ? 1 : 0"
-- Return (Cond (Infix ">" (Int 10) (Int 5)) (Int 1) (Int 0))
stmt'ret :: Stream s => S s Jstmt
stmt'ret = symbol "return" *> (Return <$> jexp)

-- | Expression statement
--
-- >>> ta stmt'expr "bean.roasted()"
-- Expr (Chain [Iden "bean",Call (Iden "roasted") []])
stmt'expr :: Stream s => S s Jstmt
stmt'expr = Expr <$> jexp

-- | Package statement
--
-- >>> ta stmt'pkg "package com.example.math"
-- Package "com.example.math"
stmt'pkg :: Stream s => S s Jstmt
stmt'pkg = Package <$> (symbol "package" *> idenpath)

-- | Import statement
--
-- >>> ta stmt'import "import java.util.*"
-- Import "java.util.*"
stmt'import :: Stream s => S s Jstmt
stmt'import = do
  _ <- symbol "import" *> option mempty (symbol "static")
  i <- idenpath
  a <- option mempty (symbol ".*")
  pure $ Import (i ++ a)

-- | Flow control statement
--
-- >>> ta stmt'flow "continue"
-- Flow "continue"
--
-- >>> ta stmt'flow "break"
-- Flow "break"
stmt'flow :: Stream s => S s Jstmt
stmt'flow = Flow <$> (symbol "continue" <|> symbol "break")

-- | Throw statement
--
-- >>> ta stmt'throw "throw new IllegalArgumentException(e)"
-- Throw (New "IllegalArgumentException" [Iden "e"])
stmt'throw :: Stream s => S s Jstmt
stmt'throw = Throw <$> (symbol "throw" *> jexp)

-- | if-statement
--
-- >>> ta stmt'if "if (a > b) {} else {}"
-- If (Infix ">" (Iden "a") (Iden "b")) (Scope "" [] []) [Else O (Scope "" [] [])]
stmt'if :: Stream s => S s Jstmt
stmt'if = do
  if'cond <- symbol "if" *> parens jexp -- if (condition)
  if' <-
    stmt'block
      <|> choice [stmt'ret, stmt'throw, stmt'flow, stmt'expr] -- if {..} or single
  elif' <- many $ do
    elif'cond <- symbol "else if" *> parens jexp -- else if (condition)
    Else elif'cond <$> stmt'block -- else if {..}
  else' <-
    option
      []
      ((: []) . Else O <$> (symbol "else" *> stmt'block)) -- else {..}
  pure $ If if'cond if' (elif' ++ else')

-- | switch statement
--
-- >>> ta stmt'switch "switch (a) {case 1: break; default: 2}"
-- Switch (Iden "a") [Case (Int 1) [Flow "break"],Case O [Expr (Int 2)]]
stmt'switch :: Stream s => S s Jstmt
stmt'switch = do
  e <- symbol "switch" *> parens jexp -- switch (expr)
  Switch e
    <$> braces
      ( some $ do
          v <-
            (symbol "case" *> jexp <* symbol ":") -- case expr:
              <|> (symbol "default" *> symbol ":" $> O) -- default:
          Case v <$> jstmts -- case body
      )

-- | try-catch statement
--
-- >>> ta stmt'try "try {} catch (Except e) {}"
-- Try (Scope "" [] []) [Catch (Iden "e") (Scope "" [] [])]
--
-- >>> ta stmt'try "try {} finally {close();}"
-- Try (Scope "" [] []) [Catch O (Scope "" [] [Expr (Call (Iden "close") [])])]
stmt'try :: Stream s => S s Jstmt
stmt'try = do
  try' <- symbol "try" *> stmt'block -- try {..}
  catch' <- many $ do
    cond' <- symbol "catch" *> parens (typ *> jexp) -- catch (Exception e)
    Catch cond' <$> stmt'block -- catch {..}
  final' <-
    option
      []
      ((: []) . Catch O <$> (symbol "finally" *> stmt'block)) -- finally {..}
  pure $ Try try' (catch' ++ final')

-- | for-statement
--
-- >>> ta stmt'for "for (int i=0;i<10;i++) {}"
-- For (Scope "" [] [Assign "=" (Iden "i") (Int 0),Expr (Infix "<" (Iden "i") (Int 10)),Expr (Postfix "++" (Iden "i"))])
stmt'for :: Stream s => S s Jstmt
stmt'for = do
  let p = stmt'assign <|> stmt'expr
  let foreach = sepBy (symbol ":") p -- (int i : ix)
  let classic = sepBy (symbol ";") p -- (int i=0; i<n; i++)
  a <- symbol "for" *> (parens classic <|> parens foreach) -- for (header)
  b <- block -- for {..}
  pure $ For (Scope mempty [] (a ++ b))

-- | while/do-while statement
--
-- >>> ta stmt'while "while (a < 5) {a++;}"
-- While (Infix "<" (Iden "a") (Int 5)) (Scope "" [] [Expr (Postfix "++" (Iden "a"))])
--
-- >>> ta stmt'while "do {a++;} while (a < 5)"
-- Do (Scope "" [] [Expr (Postfix "++" (Iden "a"))]) (Infix "<" (Iden "a") (Int 5))
stmt'while :: Stream s => S s Jstmt
stmt'while =
  (symbol "while" *> parens jexp >>= \c -> While c <$> stmt'block) -- while (cond) {..}
    <|> ( symbol "do" *> stmt'block
            >>= \b ->
              (symbol "while" *> parens jexp) >>= \e -> pure $ Do b e
        ) -- do {..} while (cond)

-- | synchronized statement
stmt'sync :: Stream s => S s Jstmt
stmt'sync = symbol "synchronized" *> parens jexp >>= \c -> Sync c <$> stmt'block
