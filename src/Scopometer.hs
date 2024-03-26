{-# LANGUAGE QuasiQuotes #-}

module Scopometer where

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

o1 :: String
o1 = "class Counter {"

o2 :: String
o2 = "public class ConcurrencyProblemDemo {"

o3 :: String
o3 = "    public static void main(String[] args) throws InterruptedException {"

o4 :: String
o4 = "    public static void gcd(int n1, int n2) {"

-- | opens a new scope
-- >>> src = "public <T, U> void (final T in, U out, int[][] matrix, String... str) {"
opener :: Stream s => S s String
opener = do
  skip
  void $ many keyword
  o <- iden
  _ <- option [] args
  void $ many (lexeme alphas)
  void $ symbol "{"
  return o

-- | reserved
keyword :: Stream s => S s String
keyword =
  choice $
    symbol <$> ["protected", "private", "interface", "public", "static", "class", "void"]

-- | identifier
-- >>> iden = identifier javaDef
iden :: Stream s => S s String
iden = lexeme $ liftA2 (:) (char '_' <|> alpha) (many alphaNum)

-- | parse a list of L-value from an argument declaration
--
-- >>> ta args "(final T int, U out, int[][] matrix, String... str)"
-- ["int","out","matrix","str"]
args :: Stream s => S s [String]
args = lexeme $ parens (sepBy (symbol ",") arg)

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
-- >>> ta arg "class<T> c0ffee"
-- "c0ffee"
arg :: Stream s => S s String
arg = do
  skip
  void $ option mempty (char '@' *> alphaNums <* skip) -- annotations
  void $ option mempty (symbol "final")
  void $ alphaNums <* skip -- type or object class
  void $ option mempty (angles (many $ noneOf ",>"))
  void $ option mempty (some $ symbol "[]")
  void $ option mempty (symbol "...")
  iden

coffee :: String
coffee = "Cat cat1 = new Cat(\"Luna\", 3);"
