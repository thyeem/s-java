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
iden'arr = liftA2 (++) (iden <|> typ'prim) (option mempty ndarr)

-- | Java primitive types
typ'prim :: Jparser String
typ'prim =
  choice
    ( string
        <$> [ "boolean"
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
  string "void" -- void
    <|> ( do
            i <- -- type name
              typ'prim
                <|> ( sepBy1 (between tidy tidy (string ".")) iden
                        >>= reorg "" "" "."
                    )
            g <- option mempty (tidy *> generic) -- generic <T>
            l <- option mempty ndarr -- ndarray [][]
            v <- option mempty (tidy *> string "...") -- varargs ...
            pure $ concat [i, g, l, v]
        )

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
ndarr = many (liftA2 (++) (tidy *> symbol "[") (string "]")) >>= reorg "" "" ""

-- | Java statement terminator (;)
semi :: Jparser [String]
semi = some (symbol ";")

-- | locator: get source location where parsing begins
loc :: Jparser String -> Jparser Identifier
loc p = get'source >>= \src -> Identifier src <$> p

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
  | Eset Jexp Jexp -- expr-context assignment
  | Eswitch Jexp [Jstmt] -- expr-context switch
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
  | Scope !String [Jexp] [Jstmt] -- new scope
  | If Jexp Jstmt [Jstmt] -- if-statement
  | Else Jexp Jstmt -- else-if/else block (only valid in 'if')
  | For Jstmt -- for-statement
  | While Jexp Jstmt -- while-statement
  | Do Jstmt Jexp -- do-while-statement
  | Switch Jexp [Jstmt] -- switch-statement
  | Case [Jexp] [Jstmt] -- case statement (only valid in 'switch')
  | Yield Jexp -- yield statement (only valid in 'case')
  | Try Jstmt [Jstmt] -- try-catch-finally statement
  | Catch Jexp Jstmt -- catch block (only valid in 'try')
  | Enum [Jstmt] -- enum declaration statement
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
  term = expr (factor <|> parens term) priority
  infix' x = InfixL $ symbol x $> Infix x
  prefix x = PrefixU $ symbol x $> Prefix x
  postfix x = PostfixU $ symbol x $> Postfix x
  ternary =
    term >>= \x ->
      option x (Tern x <$> (symbol "?" *> jexp) <*> (symbol ":" *> jexp))
  priority =
    [ [prefix "-", prefix "+", prefix "!", prefix "~"]
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
    ]

-- | Expressions with the highest priority
factor :: Jparser Jexp
factor = e <|> parens e
 where
  e =
    choice
      [ expr'lam -- lambda
      , expr'iof -- instanceof
      , expr'access -- access: Class::ref().call().array[i][j].name
      , expr'arr -- init array: {1,2,3}
      , expr'set -- expr-set: (ch = in.read(buf,0,len))
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
    (Char . (: []) <$> charLit) -- Java ASCII char literal
      <|> ( Char
              <$> between
                (string "'")
                (string "'")
                (string "\\u" >>= \u -> (u ++) <$> some hexDigit)
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
-- InstOf "String" (Eset (Iden s) (Iden name))
expr'iof :: Jparser Jexp
expr'iof = token $ do
  i <- expr'access
  string "instanceof" *> gap
  ( typ'gap >>= \t ->
      expr'iden >>= \o ->
        pure $ InstOf t (Eset o i) -- Java 16+ pattern matching
    )
    <|> (typ >>= \t -> pure (InstOf t i)) -- default

-- | Type cast expression
--
-- >>> ta expr'cast "(int) floating"
-- Cast "int" (Iden floating)
expr'cast :: Jparser Jexp
expr'cast = token $ parens (token typ) >>= \t -> Cast t <$> jexp

-- | Array initialization expression
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
-- New "Coffee" [] (Scope "Coffee" [] [Scope "Ethiopia" [] []])
expr'new :: Jparser Jexp
expr'new = token $ do
  t <- string "new" *> gap *> token typ
  a <- choice [args'expr, args'arr, some (squares (jexp <|> string "" $> E))]
  New t a <$> ((Scope t [] <$> braces jstmts) <|> pure ST)

-- | Lambda expression
-- Lambda in Java consists of expr-statement and block-statement
expr'lam :: Jparser Jexp
expr'lam = token $ do
  a <- args'decl True <|> ((: []) <$> expr'iden) -- (args) or value
  _ <- symbol "->"
  Lambda a <$> ((Scope "\\" a <$> braces jstmts) <|> stmt'expr)

-- | Variable expression
expr'iden :: Jparser Jexp
expr'iden = token $ Iden <$> loc (choice [iden, symbol "this", symbol "super"])

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
expr'access = token $ do
  base <-
    choice [expr'cast, expr'new, expr'str, access'iden]
      <|> parens jexp
  ext <- many (choice [expr'dot, expr'call, expr'idx, expr'ref])
  pure $ foldl' (\acc f -> f acc) base ext
 where
  access'iden = do
    skip generic
    token
      ( Iden
          <$> loc
            ( token iden'arr
                <|> symbol "class" -- class literal
                <|> symbol "new" -- constructor ref
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

-- | L-value expression
--
-- >>> ta expr'lval "c0ffee.beans[]"
-- Dot (Iden c0ffee) (Iden beans)
expr'lval :: Jparser Jexp
expr'lval =
  token $
    (foldl1' Dot <$> sepBy1 (symbol ".") e) <* skip ndarr
 where
  e = (expr'iden <**> expr'idx) <|> expr'iden

-- | Assign statement within expression context
--
-- >>> ta expr'set "(ch = read(buf,0,3))"
-- Eset (Iden ch) (Call (Iden read) [Iden buf,Int 0,Int 3])
expr'set :: Jparser Jexp
expr'set = parens $ expr'iden >>= \i -> symbol "=" *> (Eset i <$> jexp)

-- | Switch statement within expression context (Java 12)
expr'switch :: Jparser Jexp
expr'switch = uncurry Eswitch <$> switch

-- | Parse arguments in declaration
-- If 'optType' is set, bare-type variables are admitted.
--
-- >>> ta (args'decl False) "(final U out, int[][] matrix, String... str)"
-- [Iden out,Iden matrix,Iden str]
--
-- >>> ta (args'decl True) "(out, matrix, str)"
-- [Iden out,Iden matrix,Iden str]
args'decl :: Bool -> Jparser [Jexp]
args'decl optType = token $ parens (sepBy (symbol ",") arg)
 where
  typ'x = typ'gap *> x
  x = Iden <$> (loc (token iden) <* skip ndarr)
  arg = token $ skip (many anno) *> if optType then typ'x <|> x else typ'x

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

-- | Java statement parser
jstmt :: Jparser Jstmt
jstmt = jstmt'block <|> (jstmt'simple <* skip semi)

-- | Java [statement] parser
jstmts :: Jparser [Jstmt]
jstmts =
  tidy *> many jstmt >>= \xs ->
    if null xs
      then sepBy' semi jstmt'simple -- single statement
      else pure xs -- multiple statement

-- | Common block parser
block :: Jparser [Jstmt]
block = braces jstmts <* skip semi

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
        , stmt'ret
        , stmt'set
        , stmt'expr
        , stmt'assert
        , stmt'flow
        , stmt'throw
        ]
    )

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
-- Scope "" [] [Expr (Dot (Iden coffee) (Call (Iden roasted) []))]
block'or'single :: Jparser Jstmt
block'or'single = (Scope mempty [] <$> block) <|> jstmt

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
  ( do
      skip (choice $ string <$> ["class", "interface", "@interface"]) *> gap
      i <- token iden -- name
      g <- option mempty generic -- generic
      skipMany
        ( (symbol "extends" <|> symbol "implements")
            *> sepBy (symbol ",") (token typ)
        ) -- extends/implements
      Scope (i ++ g) mempty <$> block
    ) -- class/interface
    <|> ( do
            skip generic -- generic
            i <- (typ'gap *> token iden) <|> token iden -- method
            a <- args'decl False -- type-iden pairs in argument declaration
            skip (symbol "throws" *> sepBy (symbol ",") (token typ)) -- throws
            Scope i a <$> block
        ) -- method

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
  skip generic
  i <- typ'gap *> loc iden -- Type Name
  skip generic
  a <- args'decl False -- type-iden pairs in argument declaration
  skip (symbol "throws" *> sepBy (symbol ",") (token typ)) -- throws
  pure $ Abstract (Iden i) a

-- | Annotation element statement
--
-- >>> ta stmt'anno "int intValue() default 42"
-- AnnoEl (Iden intValue) (Int 42)
--
-- >>> ta stmt'anno "Class<?> classValue() default String.class"
-- AnnoEl (Iden classValue) (Dot (Iden String) (Iden class))
--
-- >>> ta stmt'anno "Nested nested() default @Nested(42)"
-- AnnoEl (Iden nested) (Anno (At (Call (Iden Nested) [Int 42]) []))
stmt'anno :: Jparser Jstmt
stmt'anno =
  typ'gap *> expr'iden >>= \i ->
    symbol "("
      *> symbol ")"
      *> string "default"
      *> gap
      *> (AnnoEl i <$> (jexp <|> Anno <$> anno))

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
  i <- string "enum" *> gap *> token iden -- enum Name
  skipMany
    ( (symbol "extends" <|> symbol "implements")
        *> sepBy (symbol ",") (token typ) -- extends/implements
    )
  Scope i []
    <$> braces
      ( do
          e <- Enum <$> sepBy' (symbol ",") (skip (many anno) *> enum'const)
          skip semi
          s <- jstmts
          pure $ e : s
      )
 where
  enum'const = (token iden >>= \i -> Scope i [] <$> block) <|> stmt'expr

-- | Bare-block statement
stmt'bare :: Jparser Jstmt
stmt'bare = skip (symbol "static") *> (Scope mempty [] <$> block)

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
stmt'set :: Jparser Jstmt
stmt'set = do
  skip (many anno) -- annotations
  skip (many $ modifier *> gap) -- modifiers
  ( typ'gap *> sepBy1 (symbol ",") (assign <|> decl) >>= wrap
    ) -- declare/assign: type a, b=jexp,...
    <|> assign -- assign: a=b=..=jexp
    <|> ( expr'lval >>= \i ->
            choice (symbol <$> ops) -- augmented assign operators
              >>= \op -> Set op i <$> jexp
        ) -- aug-assign: a+=jexp; a-=jexp; ...
 where
  ops = ["+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
  wrap s = if length s == 1 then pure (head s) else pure (Sets s)
  decl = flip (Set mempty) E <$> expr'lval
  assign =
    ( endBy1 (symbol "=") expr'lval
        >>= \s -> jexp >>= \e -> wrap (flip (Set "=") e <$> s)
    ) -- multiple assign
      <|> (expr'lval <* symbol "=" >>= \i -> Set "=" i <$> jexp) -- single assign

-- | Return statement
--
-- >>> ta stmt'ret "return (10 > 5) ? 1 : 0"
-- Return (Tern (Infix ">" (Int 10) (Int 5)) (Int 1) (Int 0))
stmt'ret :: Jparser Jstmt
stmt'ret = string "return" *> (Return <$> (gap *> jexp <|> pure E))

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
-- Try (Scope "" [] []) [Catch (Iden e) (Scope "" [] [])]
--
-- >>> ta stmt'try "try {} finally {close();}"
-- Try (Scope "" [] []) [Catch E (Scope "" [] [Expr (Call (Iden close) [])])]
--
-- >>> ta stmt'try "try (Buffer b = new Buffer()) {}"
-- Try (Scope "" [] [Set "=" (Iden b) (New "Buffer" [] ST)]) []
stmt'try :: Jparser Jstmt
stmt'try = do
  try' <- do
    a <-
      symbol "try"
        *> option mempty (parens (sepBy' semi stmt'set)) -- try (src)
    b <- block <|> (: []) <$> jstmt -- {..} or j-stmt;
    pure $ Scope mempty [] (a ++ b)
  catch' <- many $ do
    cond' <- -- catch (E1 | E2 e)
      symbol "catch"
        *> parens
          (typ'gap *> skipMany (symbol "|" *> typ'gap) *> expr'iden)
    Catch cond' <$> block'or'single -- catch {..}
  final' <- -- finally {..}
    option mempty ((: []) . Catch E <$> (symbol "finally" *> block'or'single))
  pure $ Try try' (catch' ++ final')

-- | if-statement
--
-- >>> ta stmt'if "if (a > b) {} else {}"
-- If (Infix ">" (Iden a) (Iden b)) (Scope "" [] []) [Else E (Scope "" [] [])]
stmt'if :: Jparser Jstmt
stmt'if = do
  if'cond <- symbol "if" *> parens jexp -- if (cond)
  if' <- block'or'single -- {..} or j-stmt;
  elif' <- many $ do
    elif'cond <- symbol "else if" *> parens jexp -- else if (cond)
    Else elif'cond <$> block'or'single -- {..} or j-stmt;
  else' <-
    option mempty ((: []) . Else E <$> (symbol "else" *> block'or'single)) -- else
  pure $ If if'cond if' (elif' ++ else')

-- | for-statement
--
-- >>> ta stmt'for "for (int i: numbers) {}"
-- For (Scope "" [] [Set "" (Iden i) E,Expr (Iden numbers)])
--
-- >>> ta stmt'for "for (int i;;) {}"
-- For (Scope "" [] [Set "" (Iden i) E,ST,ST])
--
-- >>> ta stmt'for "for (;;) { infinite.run() }"
-- For (Scope "" [] [ST,ST,ST,Expr (Dot (Iden infinite) (Call (Iden run) []))])
stmt'for :: Jparser Jstmt
stmt'for = do
  a <- symbol "for" *> (parens classic <|> parens foreach) -- for (header)
  b <- block <|> (: []) <$> jstmt -- {..} or j-stmt;
  pure $ For (Scope mempty [] (a ++ b))
 where
  p = stmt'set <|> stmt'expr
  foreach = sepBy (symbol ":") p -- (int i : ix)
  classic = sepBy (symbol ";") (p <|> (tidy $> ST)) -- (int i=0; i<n; i++)

-- | while statement
--
-- >>> ta stmt'while "while (a < 5) {a++;}"
-- While (Infix "<" (Iden a) (Int 5)) (Scope "" [] [Expr (Postfix "++" (Iden a))])
stmt'while :: Jparser Jstmt
stmt'while =
  symbol "while" *> parens jexp >>= \c ->
    While c <$> block'or'single -- while (cond) ({..} or j-stmt;)

-- | do-while statement
--
-- >>> ta stmt'do "do {a++;} while (a < 5)"
-- Do (Scope "" [] [Expr (Postfix "++" (Iden a))]) (Infix "<" (Iden a) (Int 5))
stmt'do :: Jparser Jstmt
stmt'do =
  symbol "do" *> block'or'single >>= \b ->
    (symbol "while" *> parens jexp) >>= \e ->
      pure $ Do b e -- do ({..} or j-stmt;) while (cond)

-- | switch statement
--
-- >>> ta stmt'switch "switch (a) { case 1: break; default: 2 }"
-- Switch (Iden a) [Case [Int 1] [Flow "break"],Case [E] [Expr (Int 2)]]
--
-- >>> ta stmt'switch "switch (e) { case Xcode x -> a; default -> b; }"
-- Switch (Iden e) [Case [Eset (Iden x) E] [Expr (Iden a)],Case [E] [Expr (Iden b)]]
stmt'switch :: Jparser Jstmt
stmt'switch = uncurry Switch <$> switch <* skip semi

switch :: Jparser (Jexp, [Jstmt])
switch = do
  e <- symbol "switch" *> parens jexp -- switch (expr)
  b <-
    braces
      ( some $
          Case
            <$> ( (string "case" *> gap *> label <* to) -- case expr [,expr]:
                    <|> (symbol "default" *> to $> [E]) -- default:
                ) -- case label
            <*> ( ((: []) . Yield <$> (string "yield" *> gap *> jexp)) -- yield
                    <|> jstmts
                ) -- case body
      ) -- switch body
  pure (e, b)
 where
  label = sepBy1 (symbol ",") (choice [expr'match, expr'lval, expr'prim])
  to = symbol ":" <|> symbol "->" -- Java 12+ case arrow
  expr'match = flip Eset E <$> (typ'gap *> expr'iden) -- Java 17+ pattern

-- | assert statement
stmt'assert :: Jparser Jstmt
stmt'assert = do
  string "assert" *> gap
  cond <- jexp
  Assert cond <$> ((symbol ":" *> jexp) <|> pure E)

-- | synchronized statement
stmt'sync :: Jparser Jstmt
stmt'sync =
  symbol "synchronized" *> parens jexp >>= \c ->
    Sync c <$> block'or'single

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
-- At (Iden Outer) [Pair (Iden inner) (At (Call (Iden Inner) []) [])]
--
-- >>> ta anno "@j.Query(key=sofia.maria)"
-- At (Dot (Iden j) (Iden Query)) [Pair (Iden key) (Val (Dot (Iden sofia) (Iden maria)))]
anno :: Jparser Annotation
anno =
  char '@' -- sigil
    *> ( At
          <$> expr'access -- name of annotation
          <*> option mempty (parens (sepBy' (symbol ",") unit)) -- (..)
       )
 where
  unit = choice [anno, arr, pair, val]
  arr = Arr <$> braces (sepBy' (symbol ",") (choice [anno, val]))
  pair = expr'iden >>= \k -> symbol "=" *> (Pair k <$> val)
  val = choice [anno, arr, Val <$> jexp]

-- TODO: Java 17+ record, and sealed
--
-- public sealed class Shape permits Circle, Square, Rectangle {}
-- public record Point(int x, int y) {}
