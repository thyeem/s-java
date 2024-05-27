module Code.SS.Parser where

import Control.Applicative ((<**>))
import Data.Functor (($>))
import Data.List (foldl', foldl1', intercalate)
import Text.S
  ( Lexer (..)
  , Operator (..)
  , Pretty (..)
  , S (..)
  , Source (..)
  , alphaNum
  , between
  , binary
  , char
  , choice
  , endBy1
  , expr
  , float
  , genLexer
  , get'source
  , hexDigit
  , hexadecimal
  , integer
  , javaSpec
  , many
  , oneOf
  , option
  , sepBy
  , sepBy'
  , sepBy1
  , skip
  , skipMany
  , some
  , string
  , try
  , (<|>)
  )

-- $setup
-- >>> import Text.S

type Jparser = S String

-- | Derive Java lexical unit parser from the Java language specification
lexer :: Lexer String a
lexer = genLexer javaSpec

-- | Skip whitespaces and comments
tidy :: Jparser ()
tidy = tidy' lexer

-- | Ensures being separated afterwards by whitespaces or comments
gap :: Jparser ()
gap = gap' lexer

-- | Parser builder for Java token
token :: Jparser a -> Jparser a
token = token' lexer

-- | Parser for given strings
symbol :: String -> Jparser String
symbol = symbol' lexer

-- | Parser for ( .. )
parens :: Jparser a -> Jparser a
parens = parens' lexer

-- | Parser for { .. }
braces :: Jparser a -> Jparser a
braces = braces' lexer

-- | Parser for [ .. ]
squares :: Jparser a -> Jparser a
squares = squares' lexer

-- | Parser for char literal, 'c'
charLit :: Jparser Char
charLit = charLit' lexer

-- | Parser for string literal, "string"
stringLit :: Jparser String
stringLit = stringLit' lexer

-- | identifier
--
-- >>> ta iden "_c0ffee"
-- "_c0ffee"
iden :: Jparser String
iden = identifier' lexer

-- | identifier optionally appended with n-dimentional 'array'
iden'arr :: Jparser String
iden'arr = (++) <$> (iden <|> typ'prim) <*> option mempty ndarr

-- | Java primitive types
typ'prim :: Jparser String
typ'prim =
  choice
    ( string
        <$> [ "void"
            , "boolean"
            , "byte"
            , "char"
            , "short"
            , "int"
            , "long"
            , "float"
            , "double"
            ]
    )

-- | modifier
modifier :: Jparser String
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
          , "strictfp"
          , "volatile"
          ]

-- | assignment operators
set'ops :: Jparser String
set'ops =
  choice
    ( symbol
        <$> [ "="
            , "+="
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
            ]
    )

-- | type
-- >>> ta typ "int[][] c0ffee"
-- "int[][]"
--
-- >>> ta typ "Drinks<T> c0ffee"
-- "Drinks<T>"
--
-- >>> ta typ "String... c0ffee"
-- "String..."
typ :: Jparser String
typ =
  concat
    <$> sequence
      [ typ'prim
          <|> ( sepBy1 (between tidy tidy (string ".")) iden
                  >>= reorg "" "" "."
              ) -- type name
      , option mempty (tidy *> generic) -- generic <T>
      , option mempty ndarr -- ndarray [][]
      , option mempty (tidy *> string "...") -- varargs ...
      ]

-- | spacing around type parser, 'typ'
typ'gap :: Jparser String
typ'gap =
  skip (string "final" *> gap)
    *> skip (many anno)
    *> typ
    >>= \t -> if last t `elem` ">]." then tidy $> t else gap $> t

-- | generic
--
-- >>> ta generic "<T,U>"
-- "<T,U>"
--
-- >>> ta generic "<I, T extends I>"
-- "<I,T extends I>"
--
-- >>> ta generic "<E extends Comparable<E>>"
-- "<E extends Comparable<E>>"
--
-- >>> ta generic "<K, Mouse.Key<?>>"
-- "<K,Mouse.Key<?>>"
--
-- >>> ta generic "<String[]<T>>"
-- "<String[]<T>>"
--
-- >>> ta generic "<Class<? extends Awesome>>"
-- "<Class<? extends Awesome>>"
generic :: Jparser String
generic = angles (sepBy (symbol ",") (ext <|> unit)) >>= reorg "<" ">" ","
 where
  angles = between (symbol "<") (symbol ">")
  var =
    token (skip (many anno) *> sepBy1 (symbol ".") (token iden'arr))
      >>= reorg "" "" "." -- SOME.V
  unit =
    symbol "?" -- ?
      <|> (var >>= \i -> many generic >>= reorg i "" "") -- SOME.V<U>
  ext = do
    i <- var <|> symbol "?"
    e <- symbol "extends" <|> symbol "super"
    u <- unit
    pure $ unwords [i, e, u] -- T extends SOME.V<U>

-- | N-dimentional array or ndarry
--
-- >>> ta ndarr "[\t][\n\t\n][    ]"
-- "[][][]"
--
-- >>> ta ndarr "[] [ ] [ /* comment */ ]"
-- "[][][]"
ndarr :: Jparser String
ndarr =
  many ((++) <$> (tidy *> symbol "[") <*> string "]")
    >>= reorg "" "" ""

-- | Throws clause
throws :: Jparser String
throws =
  symbol "throws" *> sepBy (symbol ",") (token typ)
    >>= reorg "" "" ","

-- | Inheritance clauses
inherits :: Jparser String
inherits =
  (symbol "extends" <|> symbol "implements")
    *> sepBy (symbol ",") (token typ)
    >>= reorg "" "" ","

-- | Java statement terminator (;)
semi :: Jparser [String]
semi = some (symbol ";")
{-# INLINE semi #-}

-- | locator: get source location where parsing begins
loc :: Jparser String -> Jparser Identifier
loc = (Identifier <$> get'source <*>)
{-# INLINE loc #-}

-- | reorganize back to original-structured string form
reorg :: Applicative f => [a] -> [a] -> [a] -> [[a]] -> f [a]
reorg bra ket sep args = pure $ bra ++ intercalate sep args ++ ket
{-# INLINE reorg #-}

-- | Java identifier with source info tagged
data Identifier = Identifier Source !String

instance Show Identifier where
  show (Identifier _ x) = x

-- | Definition of Java expression
data Jexp
  = Null -- primitive null
  | Bool !String -- primitive true/false
  | Int !Integer -- primitive integer
  | Float !Double -- primitive float
  | Char !String -- char literal
  | Str !String -- string literal
  | Iden Identifier -- identifier
  | InstOf !String Jexp -- instanceOf
  | Cast !String Jexp -- type casting
  | New !String [Jexp] Jstmt -- new object
  | Lambda [Jexp] Jstmt -- lambda expression
  | Array [Jexp] -- array initialization
  | Call Jexp [Jexp] -- method invocation
  | Index Jexp [Jexp] -- array access
  | Dot Jexp Jexp -- dot field/method access
  | Ref Jexp Jexp -- method reference
  | Prefix !String Jexp -- prefix unary operator
  | Postfix !String Jexp -- postfix unary operator
  | Infix !String Jexp Jexp -- binary infix operator
  | Tern Jexp Jexp Jexp -- ternary expression
  | Eset !String Jexp Jexp -- expr-context assignment
  | Eswitch Jexp Jstmt -- expr-context switch
  | Anno Annotation -- annotation
  | E -- placeholder expression
  deriving (Show)

instance Pretty Jexp

-- | Definition of Java statement
data Jstmt
  = Package !String -- package statement
  | Import !String -- import statement
  | Abstract Jexp [Jexp] -- abstract method statement
  | Sets [Jstmt] -- multiple decl/assign statement, [Set]
  | Set !String Jexp Jexp -- decl/assign statement
  | Return Jexp -- return statement
  | Throw Jexp -- throw statement
  | Flow !String -- flow control statement
  | Expr Jexp -- expression statement
  | Scope !String [Jexp] [Jstmt] -- new `scope`
  | Block [Jstmt] -- isolated block, but not `scope`
  | If [Jstmt] Jstmt [Jstmt] -- if-statement
  | Else [Jstmt] Jstmt -- else-if/else block (only valid in `if`)
  | For [Jstmt] Jstmt -- for-statement
  | While Jexp Jstmt -- while-statement
  | Do Jstmt Jexp -- do-while-statement
  | Try [Jstmt] Jstmt [Jstmt] -- try-catch-finally statement
  | Catch Jexp Jstmt -- catch block (only valid in `try`)
  | Switch Jexp Jstmt -- switch-statement
  | Case [Jexp] [Jstmt] -- case statement (only valid in `switch`)
  | Yield Jexp -- yield statement (only valid in `case`)
  | Enum [Jstmt] -- enum-constants declaration
  | Assert Jexp Jexp -- assert statement
  | Sync Jexp Jstmt -- synchronized statement
  | AnnoEl Jexp Jexp -- annotation elements declaration
  | ST -- placeholder statement
  deriving (Show)

instance Pretty Jstmt

-- | Jave expression parser
jexp :: Jparser Jexp
jexp = ternary <|> term
 where
  term = expr (unary <|> factor <|> parens term) priority
  infix' x = InfixL (symbol x $> Infix x)
  prefix x = PrefixU (symbol x $> Prefix x)
  postfix x = PostfixU (symbol x $> Postfix x)
  ternary =
    term >>= \x ->
      option x (Tern x <$> (symbol "?" *> jexp) <*> (symbol ":" *> jexp))
  unary =
    some (choice $ symbol <$> ["-", "+", "!", "~"])
      <**> (foldr Prefix <$> (factor <|> parens term))
  priority =
    [ [postfix "++", postfix "--", prefix "++", prefix "--"]
    , [infix' "*", infix' "/", infix' "%"]
    , [infix' "+", infix' "-"]
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
    ]

-- | Expressions with the highest priority
factor :: Jparser Jexp
factor = e <|> parens e
 where
  e =
    choice
      [ expr'lam -- lambda
      , expr'iof -- instanceof
      , expr'set -- expr-set: ch = in.read(buf,0,len)
      , expr'access -- access: Class::ref().call().array[i][j].name
      , expr'arr -- init array: {1,2,3}
      , expr'switch -- expr-switch: return switch (v) {}
      , expr'prim -- primitive
      ]

-- | primitive
expr'prim :: Jparser Jexp
expr'prim = choice [expr'chr, expr'bool, expr'null, expr'flt, expr'int, expr'str]

-- | null
expr'null :: Jparser Jexp
expr'null = symbol "null" $> Null

-- | bool
expr'bool :: Jparser Jexp
expr'bool = Bool <$> (symbol "true" <|> symbol "false")

-- | int
expr'int :: Jparser Jexp
expr'int =
  token $
    Int <$> choice [hexadecimal, binary, integer] <* skip (oneOf "lLdDfF")

-- | float
expr'flt :: Jparser Jexp
expr'flt = token $ Float <$> float <* skip (oneOf "lLdDfF")

-- | char literal
expr'chr :: Jparser Jexp
expr'chr =
  token $
    (Char . pure <$> charLit) -- Java ASCII char literal
      <|> ( Char
              <$> between
                (string "'")
                (string "'")
                ((++) <$> string "\\u" <*> some hexDigit)
          ) -- Java BMP Unicode escape sequence

-- | string literal
expr'str :: Jparser Jexp
expr'str = token $ Str <$> stringLit

-- | Instanceof expression
--
-- >>> ta expr'iof "name instanceof String"
-- InstOf "String" (Iden name)
--
-- >>> ta expr'iof "name instanceof String s"
-- InstOf "String" (Eset "=" (Iden s) (Iden name))
expr'iof :: Jparser Jexp
expr'iof = token $ do
  i <- expr'access -- iden
  string "instanceof" *> gap -- instanceof
  (try typ'gap >>= \t -> InstOf t <$> expr'match i) -- Java 16+: Type t
    <|> (typ >>= \t -> pure (InstOf t i)) -- Type

-- | Type cast expression
--
-- >>> ta expr'cast "(int) floating"
-- Cast "int" (Iden floating)
expr'cast :: Jparser Jexp
expr'cast = token $ Cast <$> parens (token typ) <*> jexp

-- | Array initialization expression
--
-- >>> ta expr'arr "{1,2,3,}"
-- Array [Int 1,Int 2,Int 3]
expr'arr :: Jparser Jexp
expr'arr = Array <$> args'arr

-- | Object creation expression
--
-- >>> ta expr'new "new Object(\"obj\", 'Q', 12.34)"
-- New "Object" [Str "obj",Char "Q",Float 12.34] ST
--
-- >>> ta expr'new "new int[]{1, 2, 3}"
-- New "int[]" [Int 1,Int 2,Int 3] ST
--
-- >>> ta expr'new "new byte[len / 2]"
-- New "byte" [Infix "/" (Iden len) (Int 2)] ST
--
-- >>> ta expr'new "new Coffee() { Bean Ethiopia() {} }"
-- New "Coffee" [] (Block [Scope "Ethiopia" [] []])
expr'new :: Jparser Jexp
expr'new =
  token $
    New -- new iden ((arg,), {e,}, [i][j]) {..}
      <$> (string "new" *> gap *> token typ)
      <*> choice
        [ args'expr -- (expr,..)
        , args'arr -- {expr,..}
        , some (squares (jexp <|> symbol "" $> E)) -- [expr]
        ]
      <*> option ST (Block <$> braces jparser) -- optional {..}

-- | Lambda expression
-- Lambda in Java consists of expr-statement and block-statement
expr'lam :: Jparser Jexp
expr'lam =
  token $
    (args'decl False <|> (pure <$> expr'iden)) >>= \a ->
      Lambda a -- (arg,..) or val
        <$> ( symbol "->"
                *> ((Scope "\\" a <$> braces jparser) <|> stmt'expr)
            ) -- {..} or j-expr

-- | Variable expression
expr'iden :: Jparser Jexp
expr'iden =
  token $ Iden <$> loc (choice [iden, symbol "this", symbol "super"])

-- | Access expression
--
-- This expression parser includes:
--   - identifier
--   - class literal: String[].class
--   - method call: call()
--   - array index access: array[i][j]
--   - field access (.)
--   - method reference (::)
--
-- >>> ta expr'access "coffee.espresso(25)"
-- Dot (Iden coffee) (Call (Iden espresso) [Int 25])
--
-- >>> ta expr'access "Class::coffee"
-- Ref (Iden Class) (Iden coffee)
--
-- >>> ta expr'access "this.coffee"
-- Dot (Iden this) (Iden coffee)
expr'access :: Jparser Jexp
expr'access =
  token $
    foldl' (\acc f -> f acc)
      <$> ( choice [expr'cast, expr'new, expr'str, access'iden]
              <|> parens jexp
          ) -- base
      <*> many
        ( choice
            [ expr'dot -- (.)
            , expr'call -- call()
            , expr'idx -- [i][j]
            , expr'ref -- (::)
            ]
        ) -- extention
 where
  access'iden =
    skip generic
      *> ( Iden
            <$> loc
              ( choice
                  [ token iden'arr -- String[]
                  , symbol "class" -- class literal
                  , symbol "new" -- new::method
                  ]
              )
         )
        <|> expr'iden

-- | Extension of call expression
expr'call :: Jparser (Jexp -> Jexp)
expr'call = flip Call <$> args'expr

-- | Extension of index access expression
expr'idx :: Jparser (Jexp -> Jexp)
expr'idx = flip Index <$> some (squares jexp)

-- | Extension of dot access expression
expr'dot :: Jparser (Jexp -> Jexp)
expr'dot = flip Dot <$> (symbol "." *> expr'access)

-- | Extension of method reference expression
expr'ref :: Jparser (Jexp -> Jexp)
expr'ref = flip Ref <$> (symbol "::" *> expr'access)

-- | L-value expression that can be assigned values
--
-- >>> ta expr'lval "c0ffee.beans[]"
-- Dot (Iden c0ffee) (Iden beans)
expr'lval :: Jparser Jexp
expr'lval =
  token $
    foldl1' Dot
      <$> sepBy1 (symbol ".") ((expr'iden <**> expr'idx) <|> expr'iden)
      <* skip ndarr

-- | Assign statement within expression context
--
-- >>> ta expr'set "ch = read(buf,0,3)"
-- Eset "=" (Iden ch) (Call (Iden read) [Iden buf,Int 0,Int 3])
expr'set :: Jparser Jexp
expr'set = assign Eset

-- | Generalized assign-statement parser including augmented assignments
assign :: (String -> Jexp -> Jexp -> a) -> Jparser a
assign f = expr'lval >>= \i -> f <$> set'ops <*> pure i <*> jexp

-- | Java 16+ pattern match
expr'match :: Jexp -> Jparser Jexp
expr'match e = flip (Eset "=") e <$> (typ'gap *> expr'iden)

-- | Switch statement within expression context (Java 12)
expr'switch :: Jparser Jexp
expr'switch = uncurry Eswitch <$> switch

-- | Parse arguments in declaration
-- If 'useType' is set, bare-type variables are admitted.
--
-- >>> ta (args'decl True) "(final U out, int[][] matrix, String... str)"
-- [Iden out,Iden matrix,Iden str]
--
-- >>> ta (args'decl False) "(out, matrix, str)"
-- [Iden out,Iden matrix,Iden str]
args'decl :: Bool -> Jparser [Jexp]
args'decl useType =
  token $
    parens
      ( sepBy
          (symbol ",")
          (skip (many anno) *> if useType then typ'x else typ'x <|> x)
      )
 where
  typ'x = typ'gap *> x
  x = token $ Iden <$> (loc (token iden) <* skip ndarr)

-- | Parse arguments of expression
--
-- >>> ta args'expr "(a,b,c)"
-- [Iden a,Iden b,Iden c]
args'expr :: Jparser [Jexp]
args'expr = token $ parens (sepBy (symbol ",") jexp)

-- | Parse array initialization expression
--
-- >>> ta args'arr "{1,2,3,}"
-- [Int 1,Int 2,Int 3]
args'arr :: Jparser [Jexp]
args'arr = token $ braces (sepBy' (symbol ",") jexp)

-- | Java code parser: the top-level wrapper parser
jparser :: Jparser [Jstmt]
jparser =
  tidy *> many jstmt >>= \xs ->
    if null xs
      then sepBy' semi jstmt'simple -- single statement
      else pure xs -- multiple statement

-- | Java statement parser
jstmt :: Jparser Jstmt
jstmt = jstmt'block <|> (jstmt'simple <* semi)

-- | Java statement that can have a curly-braced block
jstmt'block :: Jparser Jstmt
jstmt'block =
  between
    tidy
    tidy
    ( choice
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
    )

-- | Java simple statement that requires semicolon at the end
jstmt'simple :: Jparser Jstmt
jstmt'simple =
  between
    tidy
    tidy
    ( choice
        [ stmt'package
        , stmt'import
        , stmt'do
        , stmt'anno
        , stmt'abst
        , stmt'yield
        , stmt'set
        , stmt'ret
        , stmt'assert
        , stmt'flow
        , stmt'throw
        , stmt'expr
        ]
    )

-- | Parse either a braces block or Java's single statement
--
-- This parser is for:
--
-- 1. control-statement(if, swith, while, for, ..)
-- 1. try-catch
-- 1. static/synchronized block
-- 1. bare block
--
-- But these are not be used for:
-- 1. class/interface
-- 1. enum
-- 1. method
-- 1. lambda blocks.
--
-- They all are defined as 'Scope`, rather than 'Block'
--
-- >>> ta block'or'single "{ coffee.roasted(); }"
-- Block [Expr (Dot (Iden coffee) (Call (Iden roasted) []))]
block'or'single :: Jparser Jstmt
block'or'single = (Block <$> block) <|> jstmt

-- | Common block parser
block :: Jparser [Jstmt]
block = braces jparser <* skip semi

-- | New scope statment (class or method)
--
-- >>> ta stmt'scope "public static byte[] hexStringToByteArray(String str) {}"
-- Scope "hexStringToByteArray" [Iden str] []
--
-- >>> ta stmt'scope "static void main(String[] args) throws Exception {}"
-- Scope "main" [Iden args] []
--
-- >>> ta stmt'scope "public class Pair<T, U> { T first; U second; }"
-- Scope "Pair<T,U>" [] [Set "" (Iden first) E,Set "" (Iden second) E]
--
-- >>> ta stmt'scope "public <T, U> void fn(final T in, U out, int[][] matrix, String... str) {}"
-- Scope "fn" [Iden in,Iden out,Iden matrix,Iden str] []
--
-- >>> ta stmt'scope "public static void mod(int a, int b) { return a % b; }"
-- Scope "mod" [Iden a,Iden b] [Return (Infix "%" (Iden a) (Iden b))]
--
-- >>> ta stmt'scope "private static String toHexString(byte[] bytes) { 3 + 5; }"
-- Scope "toHexString" [Iden bytes] [Expr (Infix "+" (Int 3) (Int 5))]
--
-- >>> ta stmt'scope "class Ethiopia implements Q<T> { void drip(Coffee bean) {} }"
-- Scope "Ethiopia" [] [Scope "drip" [Iden bean] []]
--
-- >>> ta stmt'scope "public class M extends A<S> implements List<S>, Nice {}"
-- Scope "M" [] []
stmt'scope :: Jparser Jstmt
stmt'scope = do
  skip (many anno) -- annotations
  skip (many $ modifier *> gap) -- modifiers
  class'interface <|> method
 where
  class'interface =
    Scope
      <$> ( choice (string <$> ["class", "interface", "@interface"])
              *> gap
              *> ((++) <$> token iden <*> option mempty generic) -- name
          )
      <*> pure []
      <*> (skipMany inherits *> block) -- skip extends/implements then {..}
  method =
    Scope
      <$> (skip generic *> ((typ'gap *> token iden) <|> token iden)) -- name
      <*> args'decl True -- arguments
      <*> (skip throws *> block) -- skip throws then {..}

-- | Abstract method statement
--
-- >>> ta stmt'abst "public abstract void makeSound()"
-- Abstract (Iden makeSound) []
--
-- >>> ta stmt'abst "protected abstract double area(double scale)"
-- Abstract (Iden area) [Iden scale]
--
-- >>> ta stmt'abst "public abstract <T> T draw()"
-- Abstract (Iden draw) []
stmt'abst :: Jparser Jstmt
stmt'abst = do
  skip (many anno) -- annotations
  skip (many $ modifier *> gap) -- modifiers
  skip (choice (string <$> ["abstract", "static", "default"]) *> gap)
  Abstract -- Type name (T t,..);
    <$> ( skip generic
            *> typ'gap
            *> expr'iden -- method name
            <* skip generic
        )
    <*> (args'decl True <* skip throws) -- (T t, U u,..) [throws ..]

-- | Annotation element statement
--
-- >>> ta stmt'anno "int intValue() default 42"
-- AnnoEl (Iden intValue) (Int 42)
--
-- >>> ta stmt'anno "Class<?> classValue() default String.class"
-- AnnoEl (Iden classValue) (Dot (Iden String) (Iden class))
--
-- >>> ta stmt'anno "Nested nested() default @Nested(42)"
-- AnnoEl (Iden nested) (Anno (At (Iden Nested) [Val (Int 42)]))
stmt'anno :: Jparser Jstmt
stmt'anno =
  AnnoEl -- Type name () default expr;
    <$> (typ'gap *> expr'iden)
    <*> ( symbol "("
            *> symbol ")"
            *> string "default"
            *> gap
            *> (jexp <|> Anno <$> anno)
        )

-- | enum statement
--
-- >>> ta stmt'enum "enum Color {RED, GREEN, BLUE,}"
-- Scope "Color" [] [Enum [Expr (Iden RED),Expr (Iden GREEN),Expr (Iden BLUE)]]
--
-- >>> ta stmt'enum "enum Color {RED, GREEN, BLUE,;}"
-- Scope "Color" [] [Enum [Expr (Iden RED),Expr (Iden GREEN),Expr (Iden BLUE)]]
stmt'enum :: Jparser Jstmt
stmt'enum = do
  skip (many anno) -- annotations
  skip (many $ modifier *> gap) -- modifiers
  Scope -- enum name { enum-constants + others }
    <$> (string "enum" *> gap *> token iden <* skipMany inherits) -- enum name
    <*> pure [] -- no arguments
    <*> ( braces
            ( (:)
                <$> ( Enum
                        <$> sepBy' (symbol ",") (skip (many anno) *> enum'const)
                        <* skip semi
                    ) -- enum constants
                <*> jparser -- other fields, constructor, methods
            )
            <* skip semi
        ) -- enum body {..}
 where
  enum'const =
    (Scope <$> token iden <*> option mempty args'expr <*> block)
      <|> stmt'expr

-- | Bare-block statement
stmt'bare :: Jparser Jstmt
stmt'bare = Block <$> (skip (symbol "static") *> block)

-- | Assignment statement
--
-- >>> ta stmt'set "int number"
-- Set "" (Iden number) E
--
-- >>> ta stmt'set "int number = 5"
-- Set "=" (Iden number) (Int 5)
--
-- >>> ta stmt'set "int a, b=5"
-- Sets [Set "" (Iden a) E,Set "=" (Iden b) (Int 5)]
--
-- >>> ta stmt'set "a=b=1"
-- Sets [Set "=" (Iden a) (Int 1),Set "=" (Iden b) (Int 1)]
stmt'set :: Jparser Jstmt
stmt'set = do
  skip (many anno) -- annotations
  skip (many $ modifier *> gap) -- modifiers
  choice
    [ mixed >>= wrap -- declare/assign: Type a, b=jexp,...
    , chain'set >>= wrap -- chain set: a=b=c=...=jexp
    , assign Set -- (augmented) assign: a=jexp; a+=jexp; a-=jexp; ...
    ]
 where
  wrap s = if length s == 1 then pure (head s) else pure (Sets s)
  decl = Set mempty <$> expr'lval <*> pure E
  mixed = typ'gap *> sepBy1 (symbol ",") (assign Set <|> decl)
  chain'set =
    endBy1 (symbol "=") expr'lval <**> (fmap . flip (Set "=") <$> jexp)

-- | Return statement
--
-- >>> ta stmt'ret "return (10 > 5) ? 1 : 0"
-- Return (Tern (Infix ">" (Int 10) (Int 5)) (Int 1) (Int 0))
stmt'ret :: Jparser Jstmt
stmt'ret = Return <$> (string "return" *> (gap *> jexp <|> pure E))

-- | Expression statement
--
-- >>> ta stmt'expr "bean.roasted()"
-- Expr (Dot (Iden bean) (Call (Iden roasted) []))
stmt'expr :: Jparser Jstmt
stmt'expr = Expr <$> jexp

-- | Package statement
--
-- >>> ta stmt'package "package com.example.math"
-- Package "com.example.math"
stmt'package :: Jparser Jstmt
stmt'package =
  Package
    <$> ( skip (many anno)
            *> string "package"
            *> gap
            *> many (alphaNum <|> oneOf "$_.*")
        )

-- | Import statement
--
-- >>> ta stmt'import "import java.util.*"
-- Import "java.util.*"
--
-- >>> ta stmt'import "import com.google.gson.internal.$Gson$Types"
-- Import "com.google.gson.internal.$Gson$Types"
stmt'import :: Jparser Jstmt
stmt'import =
  Import
    <$> ( string "import"
            *> gap
            *> skip (string "static" *> gap)
            *> many (alphaNum <|> oneOf "$_.*")
        )

-- | Flow control statement
--
-- >>> ta stmt'flow "continue"
-- Flow "continue"
--
-- >>> ta stmt'flow "break"
-- Flow "break"
stmt'flow :: Jparser Jstmt
stmt'flow = Flow <$> (symbol "continue" <|> symbol "break")

-- | Throw statement
--
-- >>> ta stmt'throw "throw new IllegalArgumentException(e)"
-- Throw (New "IllegalArgumentException" [Iden e] ST)
stmt'throw :: Jparser Jstmt
stmt'throw = Throw <$> (string "throw" *> gap *> jexp)

-- | try-catch statement
--
-- >>> ta stmt'try "try {} catch (Except e) {}"
-- Try [] (Block []) [Catch (Iden e) (Block [])]
--
-- >>> ta stmt'try "try {} finally {close();}"
-- Try [] (Block []) [Catch E (Block [Expr (Call (Iden close) [])])]
--
-- >>> ta stmt'try "try (Buffer b = new Buffer()) {}"
-- Try [Set "=" (Iden b) (New "Buffer" [] ST)] (Block []) []
stmt'try :: Jparser Jstmt
stmt'try =
  Try -- try (with-resources) ({..} or j-stmt) (catch + finally)
    <$> ( symbol "try"
            *> option
              mempty
              (parens (sepBy' (symbol ";") stmt'set))
        ) -- (with-resources)
    <*> block'or'single -- {..} or j-stmt;
    <*> ( (++)
            <$> many -- (opt) multiple catch
              ( Catch
                  <$> ( symbol "catch"
                          *> parens (sepBy1 (symbol "|") typ'gap *> expr'iden)
                      ) -- catch (E1 | E2 e)
                  <*> block'or'single --  {..} or j-stmt;
              )
            <*> option -- (opt) finally
              mempty
              (pure . Catch E <$> (symbol "finally" *> block'or'single))
        ) -- catch + finally

-- | if-statement
--
-- >>> ta stmt'if "if (a > b) {} else {}"
-- If [Expr (Infix ">" (Iden a) (Iden b))] (Block []) [Else [] (Block [])]
stmt'if :: Jparser Jstmt
stmt'if =
  If -- if (cond) ({..} or j-stmt) ( else-if + else )
    <$> (symbol "if" *> parens cond) -- if (cond)
    <*> block'or'single -- if-body {..} or j-stmt;
    <*> ( (++)
            <$> many
              ( Else
                  <$> (symbol "else if" *> parens cond) -- else-if (cond)
                  <*> block'or'single -- else-if body {..} or j-stmt;
              ) -- (opt) multiple else-if
            <*> option
              mempty
              ( pure . Else mempty
                  <$> (symbol "else" *> block'or'single)
              ) -- (opt) else
        ) -- else-if + else
 where
  cond = sepBy (symbol ";") (stmt'set <|> stmt'expr) -- Java 14+

-- | for-statement
--
-- >>> ta stmt'for "for (int i: numbers) {}"
-- For [Set "" (Iden i) E,Expr (Iden numbers)] (Block [])
--
-- >>> ta stmt'for "for (int i;;) {}"
-- For [Set "" (Iden i) E,ST,ST] (Block [])
--
-- >>> ta stmt'for "for (;;) { infinite.run() }"
-- For [ST,ST,ST] (Block [Expr (Dot (Iden infinite) (Call (Iden run) []))])
stmt'for :: Jparser Jstmt
stmt'for =
  For -- for (header) ({..} or j-stmt;)
    <$> (symbol "for" *> (parens classic <|> parens foreach)) -- (header)
    <*> block'or'single -- {..} or j-stmt;
 where
  p = stmt'set <|> stmt'expr
  foreach = sepBy (symbol ":") p -- (int i : ix)
  classic = sepBy (symbol ";") (p <|> (tidy $> ST)) -- (int i=0; i<n; i++)

-- | while statement
--
-- >>> ta stmt'while "while (a < 5) {a++;}"
-- While (Infix "<" (Iden a) (Int 5)) (Block [Expr (Postfix "++" (Iden a))])
stmt'while :: Jparser Jstmt
stmt'while =
  While -- while (cond) ({..} or j-stmt;)
    <$> (symbol "while" *> parens jexp)
    <*> block'or'single

-- | do-while statement
--
-- >>> ta stmt'do "do {a++;} while (a < 5)"
-- Do (Block [Expr (Postfix "++" (Iden a))]) (Infix "<" (Iden a) (Int 5))
stmt'do :: Jparser Jstmt
stmt'do =
  Do -- do ({..} or j-stmt;) while (cond)
    <$> (symbol "do" *> block'or'single)
    <*> (symbol "while" *> parens jexp)

-- | switch statement
--
-- >>> ta stmt'switch "switch (a) { case 1: break; default: 2 }"
-- Switch (Iden a) (Block [Case [Int 1] [Flow "break"],Case [E] [Expr (Int 2)]])
--
-- >>> ta stmt'switch "switch (e) { case Xcode x -> a; }"
-- Switch (Iden e) (Block [Case [Eset "=" (Iden x) E] [Expr (Iden a)]])
--
-- >>> ta stmt'switch "switch (num) { case 1 -> One; default -> yield dunno; }"
-- Switch (Iden num) (Block [Case [Int 1] [Expr (Iden One)],Case [E] [Yield (Iden dunno)]])
stmt'switch :: Jparser Jstmt
stmt'switch = uncurry Switch <$> switch <* skip semi

switch :: Jparser (Jexp, Jstmt)
switch =
  (,)
    <$> (symbol "switch" *> parens jexp) -- switch (expr)
    <*> ( Block
            <$> braces
              ( some $
                  Case
                    <$> ( (string "case" *> gap *> label <* to) -- case e,..
                            <|> (symbol "default" *> to $> [E]) -- default:
                        ) -- case label
                    <*> jparser -- case body
              ) -- switch body
        )
 where
  label = sepBy1 (symbol ",") (choice [expr'match E, expr'lval, expr'prim])
  to = symbol ":" <|> symbol "->" -- Java 12+ case arrow

-- | yield statement
stmt'yield :: Jparser Jstmt
stmt'yield = Yield <$> (string "yield" *> gap *> jexp)

-- | assert statement
stmt'assert :: Jparser Jstmt
stmt'assert =
  Assert -- assert jexp [:reason]
    <$> (string "assert" *> gap *> jexp)
    <*> ((symbol ":" *> jexp) <|> pure E)

-- | synchronized statement
stmt'sync :: Jparser Jstmt
stmt'sync =
  Sync -- synchronized (jexp,..) ({..} or j-stmt)
    <$> (symbol "synchronized" *> parens jexp)
    <*> block'or'single

-- | Definition of Java annotation
data Annotation
  = At Jexp [Annotation]
  | Arr [Annotation]
  | Pair Jexp Annotation
  | Val Jexp
  deriving (Show)

instance Pretty Annotation

-- | annotation
--
-- >>> ta anno "@atCafe"
-- At (Iden atCafe) []
--
-- >>> ta anno "@warnings(bool=false)"
-- At (Iden warnings) [Pair (Iden bool) (Val (Bool "false"))]
--
-- >>> ta anno "@Work(days={MON,}, hour=3)"
-- At (Iden Work) [Pair (Iden days) (Arr [Val (Iden MON)]),Pair (Iden hour) (Val (Int 3))]
--
-- >>> ta anno "@Outer(inner=@Inner())"
-- At (Iden Outer) [Pair (Iden inner) (At (Iden Inner) [])]
--
-- >>> ta anno "@j.Query(key=sofia.maria)"
-- At (Dot (Iden j) (Iden Query)) [Pair (Iden key) (Val (Dot (Iden sofia) (Iden maria)))]
anno :: Jparser Annotation
anno =
  At -- @anno(..)
    <$> (char '@' *> expr'lval) -- @ + annotation name
    <*> option mempty (parens (sepBy' (symbol ",") unit)) -- optional (..)
 where
  unit = choice [anno, pair, arr, val]
  arr = Arr <$> braces (sepBy' (symbol ",") (choice [anno, val]))
  pair = Pair <$> expr'iden <*> (symbol "=" *> choice [anno, arr, val])
  val = Val <$> jexp

-- TODO: Java 17+ 'record', and 'sealed'
--
-- public sealed class Shape permits Circle, Square, Rectangle {}
-- public record Point(int x, int y) {}
