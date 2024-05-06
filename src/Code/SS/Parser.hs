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
  , hexDigit
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
  , sepBy1
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
            g <- option mempty (jump *> generic) -- generic <T>
            l <- option mempty (some $ jump *> string "[]") -- ndarry []
            v <- option mempty (jump *> string "...") -- varargs ...
            pure $ concat [i, g, concat l, v]
        )

-- | spacing around type parser, 'typ'
typ'gap :: Stream s => S s String
typ'gap = do
  t <- typ
  if not (null t) && last t `elem` ">]." then jump else gap
  pure t

-- | generic
--
-- >>> ta generic "<T,U> c0ffee[]"
-- "<T,U>"
generic :: Stream s => S s String
generic =
  between
    (string "<")
    (string ">")
    (many $ anycharBut '>')
    >>= \p -> pure $ "<" ++ filter (/= ' ') p ++ ">"

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
  | Char String -- char literal
  | Str String -- string literal
  | Iden String -- identifier
  | Chain [Jexp] -- field/method/reference chain
  | Array [Jexp] -- array initialization
  | Index Jexp [Jexp] -- array access
  | InstOf String Jexp -- instanceOf
  | Cast String Jexp -- type casting
  | New String [Jexp] Jstmt -- new object
  | Call Jexp [Jexp] -- method invocation
  | Lambda [Jexp] Jstmt -- lambda expression
  | Prefix String Jexp -- prefix unary operator
  | Postfix String Jexp -- postfix unary operator
  | Infix String Jexp Jexp -- binary infix operator
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
expr'chr =
  token $
    (Char . (: []) <$> charLit' javaDef) -- Java ASCII char literal
      <|> ( Char
              <$> between
                (string "'")
                (string "'")
                (string "\\u" >>= \u -> (u ++) <$> some hexDigit)
          ) -- Java BMP Unicode escape sequence

-- | string literal
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
expr'cast = token $ parens (jump *> typ <* jump) >>= \t -> Cast t <$> jexp

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
-- New "Object" [Str "obj",Char "Q",Float 12.34]
--
-- >>> ta expr'new "new int[]{1, 2, 3}"
-- New "int[]" [Int 1,Int 2,Int 3]
--
-- >>> ta expr'new "new byte[len / 2]"
-- New "byte" [Infix "/" (Iden "len") (Int 2)]
expr'new :: Stream s => S s Jexp
expr'new = token $ do
  t <- string "new" *> gap *> typ
  a <- choice [args'expr, args'arr, some (squares jexp)]
  b <- option mempty block -- with anonymous inner class/init-blocks
  pure $ New t a (Scope t [] b)

-- | Lambda expression
-- Lambda in Java consists of expr-statement and block-statement
expr'lam :: Stream s => S s Jexp
expr'lam = token $ do
  a <- args'decl True <|> ((: []) <$> expr'iden) -- (args) or value
  _ <- symbol "->"
  Lambda a <$> ((Scope "\\" a <$> block) <|> stmt'expr)

-- | Variable expression
expr'iden :: Stream s => S s Jexp
expr'iden = token $ Iden <$> choice [iden, symbol "this", symbol "super"]

-- | Method invocation expression
--
-- >>> ta expr'call "valueOf(2)"
-- Call (Iden "valueOf") [Int 2]
expr'call :: Stream s => S s Jexp
expr'call =
  token $
    skip generic *> jump *> expr'iden >>= \i -> Call i <$> args'expr

-- | Field/method chaining
--
-- >>> ta expr'chain "coffee.espresso(25)"
-- Chain [Iden "coffee",Call (Iden "espresso") [Int 25]]
--
-- >>> ta expr'chain "coffee"
-- Iden "coffee"
--
-- >>> ta expr'chain "this.coffee"
-- Chain [Iden "this",Iden "coffee"]
expr'chain :: Stream s => S s Jexp
expr'chain = token $ do
  base <-
    choice [expr'call, expr'idx, expr'iden, expr'new, expr'cast, expr'str]
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
  pair = typ'gap *> var
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
  | Assign [Jstmt] -- a list of assignments, [Set {}]
  | Set String Jexp Jexp -- each decl/assign (only valid in assign-statement)
  | Return Jexp -- return statement
  | Throw Jexp -- throw statement
  | Flow String -- flow control statement
  | Expr Jexp -- expression statement
  | Scope String [Jexp] [Jstmt] -- new scope
  | If Jexp Jstmt [Jstmt] -- if-statement
  | Else Jexp Jstmt -- else-if/else block (only valid in if-statement)
  | For Jstmt -- for-statement
  | While Jexp Jstmt -- while-statement
  | Do Jstmt Jexp -- do-while-statement
  | Switch Jexp [Jstmt] -- switch-statement
  | Case [Jexp] [Jstmt] -- case clause (only valid in switch-statement)
  | Try Jstmt [Jstmt] -- try-catch-finally statement
  | Catch Jexp Jstmt -- catch block (only valid in try-statement)
  | Enum [Jexp] -- enum declaration statement
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
  a <- many $ jstmt >>= \s -> (if semi'p s then s <$ symbol ";" else pure s)
  x <- many jstmt
  if
      | null a -> skip (symbol ";") $> x -- single statement
      | null x -> pure a -- well-formed multiple statement
      | otherwise -> symbol ";" $> a ++ x -- malformed multiple statement

-- | Check if the ST (statement terminator or semicolon ';') is required
semi'p :: Jstmt -> Bool
semi'p = \case
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

-- | Parse either a scoped brace block or Java's single statement
--
-- Here the braced block is not a class/method block, but rather a block that
-- does not actually define a new scope.
--
-- 1. control-statement(if, swith, while, for, ..)
-- 1. try-catch
-- 1. static/synchronized block
-- 1. bare block
-- These block have their own var-scopes, so this should be taken into account.
--
-- >>> ta block'or'single "{ coffee.roasted(); }"
-- Scope "" [] [Expr (Chain [Iden "coffee",Call (Iden "roasted") []])]
block'or'single :: Stream s => S s Jstmt
block'or'single = (Scope mempty [] <$> block) <|> (jstmt <* symbol ";")

-- | New scope statment (class or method)
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
  jump
  i <-
    choice
      [ typedef *> gap *> iden'typ -- class/interface iden
      , typ'gap *> iden -- Type iden
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
  i <- typ'gap *> iden'typ -- Type Name
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
--
-- >>> ta stmt'assign "int a, b=5"
-- Assign [Set "" (Iden "a") O,Set "=" (Iden "b") (Int 5)]
stmt'assign :: Stream s => S s Jstmt
stmt'assign = do
  skip (many $ modifier *> gap) -- modifiers
  ( typ'gap *> (Assign <$> sepBy1 (symbol ",") (decl'init <|> decl))
    ) -- declare/init: type a, b=jexp,...
    <|> ( expr'chain >>= \i ->
            (symbol "=" <|> choice (symbol <$> ops)) -- assign operators
              >>= \op -> jexp >>= (\o -> pure $ Assign [o]) . Set op i
        ) -- assign: a=jexp; a+=jexp; a-=jexp; ...
 where
  decl = iden'decl >>= \i -> pure $ Set mempty i O
  decl'init = iden'decl >>= \i -> symbol "=" >>= \op -> Set op i <$> jexp
  iden'decl = Iden <$> (iden <* skip (some (symbol "[]")) <* jump)
  ops = ["+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]

-- | Return statement
--
-- >>> ta stmt'ret "return (10 > 5) ? 1 : 0"
-- Return (Infix ":" (Infix "?" (Infix ">" (Int 10) (Int 5)) (Int 1)) (Int 0))
stmt'ret :: Stream s => S s Jstmt
stmt'ret = symbol "return" *> (Return <$> (jexp <|> pure O))

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

-- | try-catch statement
--
-- >>> ta stmt'try "try {} catch (Except e) {}"
-- Try (Scope "" [] []) [Catch (Iden "e") (Scope "" [] [])]
--
-- >>> ta stmt'try "try {} finally {close();}"
-- Try (Scope "" [] []) [Catch O (Scope "" [] [Expr (Call (Iden "close") [])])]
--
-- >>> ta stmt'try "try (Buffer b = new Buffer()) {}"
-- Try (Scope "" [] [Assign [Set "=" (Iden "b") (New "Buffer" [])]]) []
stmt'try :: Stream s => S s Jstmt
stmt'try = do
  try' <- do
    a <- symbol "try" *> option [] (parens $ many stmt'assign) -- try (src)
    b <- block <|> ((: []) <$> (jstmt <* symbol ";")) -- {..} or j-stmt;
    pure $ Scope mempty [] (a ++ b)
  catch' <- many $ do
    cond' <- -- catch (E1 | E2 e)
      symbol "catch"
        *> parens
          (typ'gap *> skipMany (symbol "|" *> typ'gap) *> expr'iden)
    Catch cond' <$> block'or'single -- catch {..}
  final' <- -- finally {..}
    option [] ((: []) . Catch O <$> (symbol "finally" *> block'or'single))
  pure $ Try try' (catch' ++ final')

-- | switch statement
--
-- >>> ta stmt'switch "switch (a) {case 1: break; default: 2}"
-- Switch (Iden "a") [Case [Int 1] [Flow "break"],Case [O] [Expr (Int 2)]]
stmt'switch :: Stream s => S s Jstmt
stmt'switch = do
  e <- symbol "switch" *> parens jexp -- switch (expr)
  b <-
    braces
      ( some $ do
          v <-
            ( string "case"
                *> gap
                *> sepBy1 (symbol ",") (expr'prim <|> expr'chain)
                <* to
              ) -- case expr [,expr]:
              <|> (symbol "default" *> to $> [O]) -- default:
          Case v <$> jstmts -- case body
      ) -- switch body
  pure $ Switch e b
 where
  to = symbol ":" <|> symbol "->" -- Java 12+

-- | if-statement
--
-- >>> ta stmt'if "if (a > b) {} else {}"
-- If (Infix ">" (Iden "a") (Iden "b")) (Scope "" [] []) [Else O (Scope "" [] [])]
stmt'if :: Stream s => S s Jstmt
stmt'if = do
  if'cond <- symbol "if" *> parens jexp -- if (condition)
  if' <- block'or'single -- {..} or j-stmt;
  elif' <- many $ do
    elif'cond <- symbol "else if" *> parens jexp -- else if (condition)
    Else elif'cond <$> block'or'single -- {..} or j-stmt;
  else' <-
    option [] ((: []) . Else O <$> (symbol "else" *> block'or'single)) -- else
  pure $ If if'cond if' (elif' ++ else')

-- | for-statement
--
-- >>> ta stmt'for "for (int i: numbers) {}"
-- For (Scope "" [] [Assign [Set "" (Iden "i") O],Expr (Iden "numbers")])
--
-- >>> ta stmt'for "for (int i=0;i<5;i++) {}"
-- For (Scope "" [] [Assign [Set "=" (Iden "i") (Int 0)],Expr (Infix "<" (Iden "i") (Int 5)),Expr (Postfix "++" (Iden "i"))])
stmt'for :: Stream s => S s Jstmt
stmt'for = do
  a <- symbol "for" *> (parens classic <|> parens foreach) -- for (header)
  b <- block <|> ((: []) <$> (jstmt <* symbol ";")) -- {..} or j-stmt;
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
  ( symbol "while" *> parens jexp >>= \c ->
      While c <$> block'or'single -- while (cond) ({..} or j-stmt;)
  )
    <|> ( symbol "do" *> block'or'single >>= \b ->
            (symbol "while" *> parens jexp) >>= \e -> pure $ Do b e
        ) -- do ({..} or j-stmt;) while (cond)

-- | synchronized statement
stmt'sync :: Stream s => S s Jstmt
stmt'sync = symbol "synchronized" *> parens jexp >>= \c -> Sync c <$> block'or'single
