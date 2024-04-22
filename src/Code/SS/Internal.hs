{-# LANGUAGE QuasiQuotes #-}

module Code.SS.Internal where

import qualified Data.Map as M
import Data.String.Here

-- type Lvalue = String

-- data Violation = Violation
-- { v'level :: !(Int, Int) -- (cur, orig)
-- , v'loc :: !(Int, Int) -- (line, col)
-- , v'var :: !Lvalue -- variable
-- , v'path :: !String -- scope path
-- }

-- data Scope = Scope
-- { s'path :: !String
-- , s'level :: !Int
-- , s'lmap :: M.Map Lvalue Scope
-- , s'gmap :: M.Map Lvalue Scope
-- , s'vio :: [Violation]
-- }

src0 :: String
src0 =
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
|]

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

src1 :: String
src1 =
  [here|
public class Sofia {
    public static void main(String[] args) { /* What about this */
        System.out.println("Hello Sofimarie:");  // Like this
        int age = 8;
        if (age > 20) {
            System.out.println("adult");
        } else{
            System.out.println("child or teenager");
        }
    }
    String studyHard(int books, String subject) {
        return "Oh!";
    }
}
|]

src2 :: String
src2 =
  [here|
public static void main(String[] args) { /* What about this */
    System.out.println("Hello Sofimarie:");  // Like this
    int age = 8;
    if (age > 20) {
        System.out.println("adult");
    } else{
        System.out.println("child or teenager");
    }
}
String studyHard(int books, String subject) {
    return "Oh!";
}
|]

src3 :: String
src3 =
  [here|
public static void main(String[] args) { /* What about this */
    System.out.println("Hello Sofimarie:")
}
|]

src4 :: String
src4 =
  [here|
public class Sofia {
    public static void main(String[] args) { /* What about this */
    }

    String studyHard(int books, String subject) {
        return "Oh!";
    }
}
|]

src5 :: String
src5 =
  [here|
public class Sofia {

    /* start of block-comment
    // MLC 1
    MLC 2
    MLC 3
    end of block-comment
    */

    public static void main(String[] args) { /* What about this */
        System.out.println("Hello Sofimarie:");  // Like this
        int age = 8;
        if (age > 20) {
            System.out.println("adult");
        } else{
            System.out.println("child or teenager");
        }
    }

    // comment for studyHard
    String studyHard(int books, String subject) {
        return "Oh!";
    }
}
|]
