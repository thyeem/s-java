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
  , between
  , braces'
  , char
  , charLit'
  , choice
  , cmtB'
  , cmtL'
  , endBy1
  , expr
  , floatB
  , gap'
  , hexDigit
  , hexadecimal
  , identifier'
  , integer
  , javaDef
  , liftA2
  , many
  , noneOf
  , oneOf
  , option
  , parens'
  , sepBy
  , sepBy'
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
      <$> [ "private"
          , "public"
          , "protected"
          , "static"
          , "final"
          , "abstract"
          , "default"
          , "synchronized"
          , "transient"
          ]

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
--
-- >>> ta generic "<E extends Comparable <E>> c0ffee[]"
-- "<E extends Comparable <E>>"
generic :: Stream s => S s String
generic = basic <|> ext
 where
  angles = between (symbol "<") (string ">")
  ws = skip spaces
  basic =
    angles (sepBy (symbol ",") ((iden'typ <* ws) <|> symbol "?"))
      >>= \p -> pure $ "<" ++ intercalate "," p ++ ">"
  ext = angles $ do
    i <- (iden'typ <|> symbol "?") <* ws
    e <- symbol "extends" <|> symbol "super"
    b <- iden'typ <* ws
    g <- basic <* ws
    pure $ "<" ++ unwords [i, e, b, g] ++ ">"

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

-- | field
--
-- >>> ta field "$c0ffee"
-- "$c0ffee"
field :: Stream s => S s String
field = liftA2 (:) (alpha <|> oneOf "_$") (many $ alphaNum <|> oneOf "_$")

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
  | Eset Jexp Jexp -- expression-context assignment
  | Call Jexp [Jexp] -- method invocation
  | Lambda [Jexp] Jstmt -- lambda expression
  | Prefix String Jexp -- prefix unary operator
  | Postfix String Jexp -- postfix unary operator
  | Infix String Jexp Jexp -- binary infix operator
  | E -- placeholder expression
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
      , expr'set -- expression set: (ch = in.read(buf,0,len))
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
-- New "Object" [Str "obj",Char "Q",Float 12.34] ST
--
-- >>> ta expr'new "new int[]{1, 2, 3}"
-- New "int[]" [Int 1,Int 2,Int 3] ST
--
-- >>> ta expr'new "new byte[len / 2]"
-- New "byte" [Infix "/" (Iden "len") (Int 2)] ST
--
-- >>> ta expr'new "new Coffee() { Bean Ethiopia() {} }"
-- New "Coffee" [] (Scope "Coffee" [] [Scope "Ethiopia" [] []])
expr'new :: Stream s => S s Jexp
expr'new = token $ do
  t <- string "new" *> gap *> typ
  a <- choice [args'expr, args'arr, some (squares jexp)]
  New t a <$> option ST (Scope t [] <$> braces jstmts)

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
    skip (generic *> jump) *> expr'iden >>= \i -> Call i <$> args'expr

-- | field/method chaining
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
      (symbol "." <|> symbol "::") -- access(.)/reference(::)
        *> (expr'call <|> (Iden <$> field)) -- method/field
  if null ext then pure base else pure $ Chain (base : ext)

-- | L-value candidate expression
--
-- >>> ta expr'lval "c0ffee.beans[]"
-- Chain [Iden "c0ffee",Iden "beans"]
expr'lval :: Stream s => S s Jexp
expr'lval =
  ( expr'idx <|> expr'iden >>= \b ->
      many (symbol "." *> token (Iden <$> field))
        >>= \e -> if null e then pure b else pure $ Chain (b : e)
  )
    <* skipMany (symbol "[]")

-- | Assign statement within expression context
--
-- >>> ta expr'set "(ch = read(buf,0,3))"
-- Eset (Iden "ch") (Call (Iden "read") [Iden "buf",Int 0,Int 3])
expr'set :: Stream s => S s Jexp
expr'set = parens $ expr'iden >>= \i -> symbol "=" *> (Eset i <$> jexp)

-- | Parse arguments in declaration
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
  var = Iden <$> (iden <* skipMany (symbol "[]"))
  arg =
    skip (string "final" *> gap)
      *> (if optType then pair <|> var else pair)
      <* jump

-- | Parse arguments of expression
--
-- >>> ta args'expr "(a,b,c)"
-- [Iden "a",Iden "b",Iden "c"]
args'expr :: Stream s => S s [Jexp]
args'expr = token $ parens (sepBy (symbol ",") jexp)

-- | Parse array initialization expression
--
-- >>> ta args'arr "{1,2,3,}"
-- [Int 1,Int 2,Int 3]
args'arr :: Stream s => S s [Jexp]
args'arr = token $ braces (sepBy' (symbol ",") jexp)

-- | Definition of Java statement
data Jstmt
  = Package String -- package statement
  | Import String -- import statement
  | Abstract Jexp [Jexp] -- abstract method statement
  | Sets [Jstmt] -- multiple decl/assign statement, [Set]
  | Set String Jexp Jexp -- decl/assign statement
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
  | ST -- placeholder statement
  deriving (Show)

instance Pretty Jstmt

-- | Java statement parser
jstmt :: Stream s => S s Jstmt
jstmt =
  between
    jump
    jump
    (jstmt'block <|> (jstmt'simple <* symbol ";"))

-- | Java [statement] parser
jstmts :: Stream s => S s [Jstmt]
jstmts = many jstmt >>= \x -> if not (null x) then pure x else many jstmt'simple

-- | Common block parser
block :: Stream s => S s [Jstmt]
block = braces jstmts <* skip (symbol ";")

-- | Java statement that can have a curly-braced block
jstmt'block :: Stream s => S s Jstmt
jstmt'block =
  choice
    [ stmt'if
    , stmt'for
    , stmt'while
    , stmt'switch
    , stmt'try
    , stmt'sync
    , stmt'bare
    , stmt'enum
    , stmt'scope
    ]

-- | Java simple statement that requires semicolon at the end
jstmt'simple :: Stream s => S s Jstmt
jstmt'simple =
  choice
    [ stmt'pkg
    , stmt'import
    , stmt'abs
    , stmt'ret
    , stmt'set
    , stmt'expr
    , stmt'flow
    , stmt'throw
    ]

-- | Parse either a scoped brace block or Java's single statement
--
-- This parser is for:
--
-- 1. control-statement(if, swith, while, for, ..)
-- 1. try-catch
-- 1. static/synchronized block
-- 1. bare block
--
-- But this is not be used for a class/method/lambda blocks.
--
-- >>> ta block'or'single "{ coffee.roasted(); }"
-- Scope "" [] [Expr (Chain [Iden "coffee",Call (Iden "roasted") []])]
block'or'single :: Stream s => S s Jstmt
block'or'single = (Scope mempty [] <$> block) <|> jstmt

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
  skip (generic *> jump) -- generic
  i <-
    ((string "class" <|> string "interface") *> gap *> iden'typ) -- class/interface
      <|> ((typ'gap *> iden) <|> iden'typ) -- method
  skip (generic *> jump) -- generic
  a <- option [] (args'decl False) -- type-iden pairs in argument declaration
  skipMany (choice [alphaNum, oneOf ",<>()", space]) -- throws/extends/implements
  Scope i a <$> block

-- | Abstract method statement
--
-- >>> ta stmt'scope "class Ethiopia { void drip(Coffee bean) {} }"
-- Scope "Ethiopia" [] [Scope "drip" [Iden "bean"] []]
stmt'abs :: Stream s => S s Jstmt
stmt'abs = do
  skip (choice (string <$> ["abstract", "static", "default"]) *> gap)
  i <- typ'gap *> iden'typ -- Type Name
  skip (generic *> jump)
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
  i <- string "enum" *> gap *> iden'typ <* jump -- enum Name
  Scope i []
    <$> braces
      ( do
          e <- Enum <$> sepBy' (symbol ",") jexp
          skip (symbol ";")
          s <- jstmts
          pure $ e : s
      )

-- | Bare-block statement
stmt'bare :: Stream s => S s Jstmt
stmt'bare = skip (symbol "static") *> (Scope mempty [] <$> block)

-- | Assignment statement
--
-- >>> ta stmt'set "int number"
-- Set "" (Iden "number") E
--
-- >>> ta stmt'set "int number = 5"
-- Set "=" (Iden "number") (Int 5)
--
-- >>> ta stmt'set "int a, b=5"
-- Sets [Set "" (Iden "a") E,Set "=" (Iden "b") (Int 5)]
stmt'set :: Stream s => S s Jstmt
stmt'set = do
  skip (many $ modifier *> gap) -- modifiers
  ( typ'gap *> sepBy1 (symbol ",") (assign <|> decl) >>= wrap
    ) -- multiple declare/assign: type a, b=jexp,...
    <|> assign -- multiple assign: a=b=..=jexp
    <|> ( expr'lval >>= \i ->
            choice (symbol <$> ops) -- augmented assign operators
              >>= \op -> Set op i <$> jexp
        ) -- aug-assign: a+=jexp; a-=jexp; ...
 where
  ops = ["+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
  wrap s = if length s == 1 then pure (head s) else pure (Sets s)
  decl = flip (Set mempty) E <$> expr'lval
  assign =
    endBy1 (symbol "=") expr'lval
      >>= \s -> jexp >>= \e -> wrap (flip (Set "=") e <$> s)

-- | Return statement
--
-- >>> ta stmt'ret "return (10 > 5) ? 1 : 0"
-- Return (Infix ":" (Infix "?" (Infix ">" (Int 10) (Int 5)) (Int 1)) (Int 0))
stmt'ret :: Stream s => S s Jstmt
stmt'ret = symbol "return" *> (Return <$> (jexp <|> pure E))

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
-- Throw (New "IllegalArgumentException" [Iden "e"] ST)
stmt'throw :: Stream s => S s Jstmt
stmt'throw = Throw <$> (string "throw" *> gap *> jexp)

-- | try-catch statement
--
-- >>> ta stmt'try "try {} catch (Except e) {}"
-- Try (Scope "" [] []) [Catch (Iden "e") (Scope "" [] [])]
--
-- >>> ta stmt'try "try {} finally {close();}"
-- Try (Scope "" [] []) [Catch E (Scope "" [] [Expr (Call (Iden "close") [])])]
--
-- >>> ta stmt'try "try (Buffer b = new Buffer()) {}"
-- Try (Scope "" [] [Set "=" (Iden "b") (New "Buffer" [] ST)]) []
stmt'try :: Stream s => S s Jstmt
stmt'try = do
  try' <- do
    a <- symbol "try" *> option [] (parens $ many stmt'set) -- try (src)
    b <- block <|> (: []) <$> jstmt -- {..} or j-stmt;
    pure $ Scope mempty [] (a ++ b)
  catch' <- many $ do
    cond' <- -- catch (E1 | E2 e)
      symbol "catch"
        *> parens
          (typ'gap *> skipMany (symbol "|" *> typ'gap) *> expr'iden)
    Catch cond' <$> block'or'single -- catch {..}
  final' <- -- finally {..}
    option [] ((: []) . Catch E <$> (symbol "finally" *> block'or'single))
  pure $ Try try' (catch' ++ final')

-- | switch statement
--
-- >>> ta stmt'switch "switch (a) {case 1: break; default: 2}"
-- Switch (Iden "a") [Case [Int 1] [Flow "break"],Case [E] [Expr (Int 2)]]
stmt'switch :: Stream s => S s Jstmt
stmt'switch = do
  e <- symbol "switch" *> parens jexp -- switch (expr)
  b <-
    braces
      ( some $ do
          v <-
            ( string "case"
                *> gap
                *> sepBy1 (symbol ",") (expr'lval <|> expr'prim)
                <* to
              ) -- case expr [,expr]:
              <|> (symbol "default" *> to $> [E]) -- default:
          Case v <$> jstmts -- case body
      ) -- switch body
  pure $ Switch e b
 where
  to = symbol ":" <|> symbol "->" -- Java 12+

-- | if-statement
--
-- >>> ta stmt'if "if (a > b) {} else {}"
-- If (Infix ">" (Iden "a") (Iden "b")) (Scope "" [] []) [Else E (Scope "" [] [])]
stmt'if :: Stream s => S s Jstmt
stmt'if = do
  if'cond <- symbol "if" *> parens jexp -- if (cond)
  if' <- block'or'single -- {..} or j-stmt;
  elif' <- many $ do
    elif'cond <- symbol "else if" *> parens jexp -- else if (cond)
    Else elif'cond <$> block'or'single -- {..} or j-stmt;
  else' <-
    option [] ((: []) . Else E <$> (symbol "else" *> block'or'single)) -- else
  pure $ If if'cond if' (elif' ++ else')

-- | for-statement
--
-- >>> ta stmt'for "for (int i: numbers) {}"
-- For (Scope "" [] [Set "" (Iden "i") E,Expr (Iden "numbers")])
--
-- >>> ta stmt'for "for (int i=0;i<5;i++) {}"
-- For (Scope "" [] [Set "=" (Iden "i") (Int 0),Expr (Infix "<" (Iden "i") (Int 5)),Expr (Postfix "++" (Iden "i"))])
stmt'for :: Stream s => S s Jstmt
stmt'for = do
  a <- symbol "for" *> (parens classic <|> parens foreach) -- for (header)
  b <- block <|> (: []) <$> jstmt -- {..} or j-stmt;
  pure $ For (Scope mempty [] (a ++ b))
 where
  p = stmt'set <|> stmt'expr
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
            (symbol "while" *> parens jexp)
              >>= \e -> pure $ Do b e
        ) -- do ({..} or j-stmt;) while (cond)

-- | synchronized statement
stmt'sync :: Stream s => S s Jstmt
stmt'sync = symbol "synchronized" *> parens jexp >>= \c -> Sync c <$> block'or'single
