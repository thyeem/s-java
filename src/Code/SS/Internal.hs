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

ff :: String
ff =
  [here|
//        if (args.length > 0 && args[0].equals(EXIT_AFTER_PAINT)) {
//            exitAfterFirstPaint = true;
//        }
    SwingUtilities.invokeAndWait(new Runnable() { return 3; }, 3);

//        new Runnable() {
//
//            public void run() {
//                JFrame frame = new JFrame();
//                frame.setTitle(resources.getString("Title"));
//                frame.setBackground(Color.lightGray);
//                frame.getContentPane().setLayout(new BorderLayout());
//                Notepad notepad = new Notepad();
//                frame.getContentPane().add("Center", notepad);
//                frame.setJMenuBar(notepad.createMenubar());
//                frame.addWindowListener(new AppCloser());
//                frame.pack();
//                frame.setSize(500, 600);
//                frame.setVisible(true);
//            }
//      }
//    );

|]
