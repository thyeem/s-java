{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Code.SS.Parser where

import Data.Functor (($>))
import Text.S
  ( Operator (..)
  , Pretty (..)
  , S (..)
  , Stream
  , alphaNum
  , angles'
  , anycharBut
  , between
  , braces'
  , char
  , charLit'
  , choice
  , cmtB'
  , cmtL'
  , cutBy
  , expr
  , floatB
  , gap'
  , hexadecimal
  , identifier'
  , integer
  , javaDef
  , many
  , noneOf
  , oneOf
  , option
  , parens'
  , sepBy
  , skip
  , skipMany
  , some
  , space
  , spaces
  , squares'
  , string
  , stringLit'
  , symbol'
  , token'
  , (<|>)
  )

-- $setup
-- >>> import Text.S

-- | Skip whitespaces, comments, and annotations
jump :: Stream s => S s ()
jump = skipMany $ choice [spaces, cmtL' javaDef, cmtB' javaDef, anno]

-- | Ensure being separated by spaces or comments afterwards
gap :: Stream s => S s ()
gap = gap' javaDef

-- | Parser for Java token
token :: Stream s => S s a -> S s a
token = token' jump

-- | Parser for given strings
symbol :: Stream s => String -> S s String
symbol = symbol' jump

-- | Parser for < .. >
angles :: Stream s => S s a -> S s a
angles = angles' jump

-- | Parser for ( .. )
parens :: Stream s => S s a -> S s a
parens = parens' jump

-- | Parser for { .. }
braces :: Stream s => S s a -> S s a
braces = braces' jump

-- | Parser for [ .. ]
squares :: Stream s => S s a -> S s a
squares = squares' jump

-- | modifier
modifier :: Stream s => S s String
modifier =
  choice $
    string
      <$> ["protected", "private", "public", "abstract", "default", "static", "final"]

-- | type definition keywords
typedef :: Stream s => S s String
typedef = choice $ string <$> ["class", "interface"]

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
  string "void"
    <|> ( do
            i <- iden'typ -- type name
            g <- option mempty generic -- generic <T>
            l <- option mempty (some $ string "[]") -- ndarry []
            v <- option mempty (string "...") -- varargs ...
            pure $ concat [i, g, concat l, v]
        )

-- | generic
--
-- >>> ta generic "<T,U> c0ffee[]"
-- "<T,U>"
generic :: Stream s => S s String
generic = angles (many $ anycharBut '>') >>= \p -> pure $ "<" ++ p ++ ">"

-- | annotations
--
-- >>> ta anno "@atCafe string c0ffee"
-- "@atCafe"
--
-- >>> ta anno "@SuppressWarnings(bool=false) public"
-- "@SuppressWarnings(bool=false)"
anno :: Stream s => S s String
anno = do
  c <- char '@' -- sigil
  n <- iden'typ -- name of annotation
  a <- -- optional args (...)
    option
      mempty
      ( parens (many $ stringLit' javaDef <|> some (noneOf ")\""))
          >>= \p -> pure $ "(" ++ concat p ++ ")"
      )
  pure $ c : n ++ a

-- | identifier
--
-- >>> ta iden "_c0ffee"
-- "_c0ffee"
iden :: Stream s => S s String
iden = identifier' javaDef

-- | identifier for type
--
-- >>> ta iden'typ "int"
-- "int"
iden'typ :: Stream s => S s String
iden'typ =
  choice
    ( string
        <$> ["boolean", "byte", "char", "short", "int", "long", "float", "double"]
    ) -- primitive types
    <|> iden

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
  | Prefix String Jexp -- prefix unary operator
  | Postfix String Jexp -- postfix unary operator
  | Infix String Jexp Jexp -- binary infix operator
  | This -- 'this' special keyword
  | O -- nil expression
  deriving (Show)

instance Pretty Jexp

-- | Expression in Java
jexp :: Stream s => S s Jexp
jexp = expr atom priority
 where
  atom = factor <|> parens jexp
  priority =
    [ [prefix "-", prefix "+", prefix "!"]
    , [prefix "++", prefix "--", postfix "++", postfix "--"]
    , [infix' "*", infix' "/"]
    , [infix' "+", infix' "-", infix' "%"]
    ,
      [ infix' "^"
      , infix' "&"
      , infix' "|"
      , infix' "<<"
      , infix' ">>"
      , infix' ">>>"
      ]
    ,
      [ infix' ">"
      , infix' "<"
      , infix' ">="
      , infix' "<="
      , infix' "=="
      , infix' "!="
      , infix' "&&"
      , infix' "||"
      ]
    , [infix' "?", infix' ":"] -- ternary operator as bop
    ]
  infix' sym = InfixL $ symbol sym $> Infix sym
  prefix sym = PrefixU $ symbol sym $> Prefix sym
  postfix sym = PostfixU $ symbol sym $> Postfix sym

-- | Expressions with the highest priority
factor :: Stream s => S s Jexp
factor = e <|> parens e
 where
  e =
    choice
      [ expr'iof -- instanceof
      , expr'lam -- lambda
      --    | expr'call: method()
      --    | expr'idx: arr[i]
      --    | expr'iden: identifier
      --    | expr'new: new object
      --    | expr'cast: (type) expr
      --    | expr'this: this
      --    | expr'str: "string"
      , expr'chain -- join above with '.' (access op) and ':' (method ref)
      , expr'arr -- init array: {1,2,3}
      , expr'prim -- primitive
      ]

-- | primitive
expr'prim :: Stream s => S s Jexp
expr'prim = choice [expr'chr, expr'bool, expr'null, expr'flt, expr'int, expr'str]

-- | null
expr'null :: Stream s => S s Jexp
expr'null = symbol "null" $> Null

-- | bool
expr'bool :: Stream s => S s Jexp
expr'bool = Bool <$> (symbol "true" <|> symbol "false")

-- | int
expr'int :: Stream s => S s Jexp
expr'int =
  token $
    Int <$> (string "0x" *> hexadecimal <|> (integer <* skip (oneOf "Ll")))

-- | float
expr'flt :: Stream s => S s Jexp
expr'flt = token $ Float <$> floatB <* skip (oneOf "Ff")

-- | char literal
expr'chr :: Stream s => S s Jexp
expr'chr = token $ Char <$> charLit' javaDef

-- | string literalj
expr'str :: Stream s => S s Jexp
expr'str = token $ Str <$> stringLit' javaDef

-- | Instanceof expression
--
-- >>> ta expr'iof "name instanceof String"
-- InstOf "String" (Iden "name")
expr'iof :: Stream s => S s Jexp
expr'iof = token $ do
  i <- expr'chain
  string "instanceof" *> gap
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
expr'idx = token $ expr'iden >>= \i -> Index i <$> some (squares jexp)

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
    (string "new" *> gap *> token typ)
      >>= \t -> New t <$> choice [args'expr, args'arr, some (squares jexp)]

-- | Lambda expression
-- Lambda in Java consists of expr-statement and block-statement
expr'lam :: Stream s => S s Jexp
expr'lam = token $ do
  a <- args'decl True <|> ((: []) <$> expr'iden) -- (args) or value
  _ <- symbol "->"
  Lambda a <$> ((Scope "\\" a <$> block) <|> stmt'expr)

-- | Variable expression
expr'iden :: Stream s => S s Jexp
expr'iden = token $ Iden <$> iden

-- | Keyword 'this'
-- 'this' isn't a 'jexp', but can be thought of as an expression semantically
expr'this :: Stream s => S s Jexp
expr'this = This <$ symbol "this"

-- | Method invocation expression
--
-- >>> ta expr'call "valueOf(2)"
-- Call (Iden "valueOf") [Int 2]
expr'call :: Stream s => S s Jexp
expr'call =
  token $
    skip generic *> (expr'iden <|> Iden <$> symbol "super")
      >>= \i -> Call i <$> args'expr

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
expr'chain = token $ do
  base <-
    choice [expr'call, expr'idx, expr'iden, expr'new, expr'cast, expr'this, expr'str]
  ext <-
    many $
      ( symbol "." -- access field/method
          <|> symbol "::" -- method reference
      )
        *> ( expr'call -- method call
              <|> Iden <$> some (alphaNum <|> oneOf "_$") -- identifier
           )
  if null ext then pure base else pure $ Chain (base : ext)

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
  pair = typ *> gap *> var
  var = Iden <$> (iden <* skip (some (symbol "[]")))
  arg =
    skip (string "final" *> gap)
      *> (if optType then pair <|> var else pair)
      <* jump

-- | Parse L-values from a list of 'Jexp' arguments
--
-- >>> ta args'expr "(a,b,c)"
-- [Iden "a",Iden "b",Iden "c"]
args'expr :: Stream s => S s [Jexp]
args'expr = token $ parens (sepBy (symbol ",") jexp)

-- | Parse L-values from array initialization expression
--
-- >>> ta args'arr "{1,2,3,}"
-- [Int 1,Int 2,Int 3]
args'arr :: Stream s => S s [Jexp]
args'arr = token $ braces (cutBy (symbol ",") jexp)

-- | Definition of Java statement
data Jstmt
  = Package String -- package statement
  | Import String -- import statement
  | Abstract Jexp [Jexp] -- abstract method statement
  | Scope String [Jexp] [Jstmt] -- new scope
  | Return Jexp -- return statement
  | Flow String -- flow control statement
  | Expr Jexp -- expression statement
  | Assign [Jstmt] -- a list of assignments, [Set {}]
  | Set String Jexp Jexp -- each decl/assign (only valid in assign-statement)
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
        , stmt'enum
        , stmt'scope
        , stmt'abs
        , stmt'ret
        , stmt'assign
        , stmt'expr
        , stmt'flow
        , stmt'throw
        , stmt'if
        , stmt'for
        , stmt'while
        , stmt'switch
        , stmt'try
        , stmt'sync
        , stmt'bare
        ]
    )

-- | Java [statement] parser
jstmts :: Stream s => S s [Jstmt]
jstmts = do
  a <- many $ jstmt >>= \s -> (if need'st s then s <$ symbol ";" else pure s)
  x <- many jstmt
  if
      | null a -> skip (symbol ";") $> x -- single statement
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
block = braces jstmts <* skip (symbol ";")

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
  skip (many $ modifier *> gap) -- modifiers
  skip generic -- generic after mod
  i <-
    choice
      [ typedef *> gap *> iden'typ -- class/interface iden
      , typ *> gap *> iden -- Type iden
      , iden'typ -- iden
      ]
  skip generic -- generic after iden
  jump
  a <- option [] (args'decl False) -- type-iden pairs in argument declaration
  skipMany (choice [alphaNum, oneOf ",<>()", space]) -- ignore throws/extends
  Scope i a <$> block

-- | Abstract method statement
--
-- >>> ta stmt'scope "class Ethiopia { void drip(Coffee bean) {} }"
-- Scope "Ethiopia" [] [Scope "drip" [Iden "bean"] []]
stmt'abs :: Stream s => S s Jstmt
stmt'abs = do
  skip (choice (string <$> ["abstract", "static", "default"]) *> gap)
  i <- typ *> gap *> iden'typ -- Type Name
  skip generic
  jump
  a <- args'decl False -- type-iden pairs in argument declaration
  pure $ Abstract (Iden i) a

-- | enum statement
--
-- >>> ta stmt'enum "enum Color {RED, GREEN, BLUE,}"
-- Scope "Color" [] [Enum [Iden "RED",Iden "GREEN",Iden "BLUE"]]
--
-- >>> ta stmt'enum "enum Color {RED, GREEN, BLUE,;}"
-- Scope "Color" [] [Enum [Iden "RED",Iden "GREEN",Iden "BLUE"]]
stmt'enum :: Stream s => S s Jstmt
stmt'enum = do
  skip (many $ modifier *> gap) -- modifiers
  i <- string "enum" *> gap *> iden'typ -- enum Name
  jump
  Scope i []
    <$> braces
      ( do
          e <- Enum <$> (cutBy (symbol ",") jexp <* skip (symbol ";"))
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
  skip (string "static" *> gap)
  skip (choice (string <$> ["synchronized", "final", "transient"]) *> gap)
  Scope mempty [] <$> block

-- | Assignment statement
--
-- >>> ta stmt'assign "int number"
-- Assign [Set "" (Iden "number") O]
--
-- >>> ta stmt'assign "int number = 5"
-- Assign [Set "=" (Iden "number") (Int 5)]
stmt'assign :: Stream s => S s Jstmt
stmt'assign = do
  skip (many $ modifier *> gap) -- modifiers
  ( do
      typ *> gap
      a <- decl'init
      o <- many (symbol "," *> decl'init)
      pure $ Assign (a : o) -- declare & init: type a=jexp [, b=jexp,..]
    )
    <|> ( do
            typ *> gap
            a <- decl
            o <- many (symbol "," *> decl)
            pure $ Assign (a : o) -- declare: type a [, b,..]
        )
    <|> ( expr'chain >>= \i ->
            symbol "="
              <|> choice
                ( symbol
                    <$> [ "+="
                        , "-="
                        , "*="
                        , "/="
                        , "%="
                        , "<<="
                        , ">>="
                        , ">>>="
                        , "&="
                        , "^="
                        , "|="
                        ] -- compound assignment operators
                )
              >>= \op -> jexp >>= (\o -> pure $ Assign [o]) . Set op i
        ) -- assign: a=jexp, a+=jexp, a-=jexp,..
 where
  decl = iden'decl >>= \i -> pure $ Set mempty i O
  decl'init = iden'decl >>= \i -> symbol "=" >>= \op -> Set op i <$> jexp
  iden'decl = Iden <$> (iden <* skip (some (symbol "[]")) <* jump)

-- | Return statement
--
-- >>> ta stmt'ret "return (10 > 5) ? 1 : 0"
-- Return (Infix ":" (Infix "?" (Infix ">" (Int 10) (Int 5)) (Int 1)) (Int 0))
stmt'ret :: Stream s => S s Jstmt
stmt'ret = string "return" *> gap *> (Return <$> (jexp <|> pure O))

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
stmt'pkg = Package <$> (string "package" *> gap *> many (alphaNum <|> oneOf ".*_"))

-- | Import statement
--
-- >>> ta stmt'import "import java.util.*"
-- Import "java.util.*"
stmt'import :: Stream s => S s Jstmt
stmt'import =
  Import
    <$> ( string "import"
            *> gap
            *> skip (string "static" *> gap)
            *> many (alphaNum <|> oneOf ".*_")
        )

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
stmt'throw = Throw <$> (string "throw" *> gap *> jexp)

-- | if-statement
--
-- >>> ta stmt'if "if (a > b) {} else {}"
-- If (Infix ">" (Iden "a") (Iden "b")) (Scope "" [] []) [Else O (Scope "" [] [])]
stmt'if :: Stream s => S s Jstmt
stmt'if = do
  if'cond <- string "if" *> gap *> parens jexp -- if (condition)
  if' <-
    stmt'block
      <|> choice [stmt'ret, stmt'throw, stmt'flow, stmt'expr] -- if {..} or single
  elif' <- many $ do
    elif'cond <- string "else if" *> gap *> parens jexp -- else if (condition)
    Else elif'cond <$> stmt'block -- else if {..}
  else' <-
    option [] ((: []) . Else O <$> (string "else" *> gap *> stmt'block)) -- else {..}
  pure $ If if'cond if' (elif' ++ else')

-- | switch statement
--
-- >>> ta stmt'switch "switch (a) {case 1: break; default: 2}"
-- Switch (Iden "a") [Case (Int 1) [Flow "break"],Case O [Expr (Int 2)]]
stmt'switch :: Stream s => S s Jstmt
stmt'switch = do
  e <- string "switch" *> gap *> parens jexp -- switch (expr)
  Switch e
    <$> braces -- switch body
      ( some $ do
          v <-
            (string "case" *> gap *> jexp <* symbol ":") -- case expr [,expr]:
              <|> (string "default" *> gap *> symbol ":" $> O) -- default:
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
  try' <- do
    a <- string "try" *> gap *> option [] (parens $ many stmt'assign) -- try (with)
    b <- block -- try {..}
    pure $ Scope mempty [] (a ++ b)
  catch' <- many $ do
    cond' <- -- catch (E1 | E2 e)
      string "catch"
        *> gap
        *> parens
          (typ *> gap *> skipMany (symbol "|" *> typ *> gap) *> expr'iden)
    Catch cond' <$> stmt'block -- catch {..}
  final' <- -- finally {..}
    option [] ((: []) . Catch O <$> (string "finally" *> gap *> stmt'block))
  pure $ Try try' (catch' ++ final')

-- | for-statement
--
-- >>> ta stmt'for "for (int i: numbers) {}"
-- For (Scope "" [] [Assign [Set "" (Iden "i") O],Expr (Iden "numbers")])
--
-- >>> ta stmt'for "for (int i=0;i<5;i++) {}"
-- For (Scope "" [] [Assign [Set "=" (Iden "i") (Int 0)],Expr (Infix "<" (Iden "i") (Int 5)),Expr (Postfix "++" (Iden "i"))])
stmt'for :: Stream s => S s Jstmt
stmt'for = do
  a <- string "for" *> gap *> (parens classic <|> parens foreach) -- for (header)
  b <- block -- for {..}
  pure $ For (Scope mempty [] (a ++ b))
 where
  p = stmt'assign <|> stmt'expr
  foreach = sepBy (symbol ":") p -- (int i : ix)
  classic = sepBy (symbol ";") p -- (int i=0; i<n; i++)

-- | while/do-while statement
--
-- >>> ta stmt'while "while (a < 5) {a++;}"
-- While (Infix "<" (Iden "a") (Int 5)) (Scope "" [] [Expr (Postfix "++" (Iden "a"))])
--
-- >>> ta stmt'while "do {a++;} while (a < 5)"
-- Do (Scope "" [] [Expr (Postfix "++" (Iden "a"))]) (Infix "<" (Iden "a") (Int 5))
stmt'while :: Stream s => S s Jstmt
stmt'while =
  (string "while" *> gap *> parens jexp >>= \c -> While c <$> stmt'block) -- while (cond) {..}
    <|> ( string "do" *> gap *> stmt'block
            >>= \b ->
              (string "while" *> gap *> parens jexp) >>= \e -> pure $ Do b e
        ) -- do {..} while (cond)

-- | synchronized statement
stmt'sync :: Stream s => S s Jstmt
stmt'sync = string "synchronized" *> gap *> parens jexp >>= \c -> Sync c <$> stmt'block
