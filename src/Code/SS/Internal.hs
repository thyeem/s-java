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
//    protected JButton createToolbarButton(String key) {
//        URL url = getResource(key + imageSuffix);
//
        JButton b = new JButton(new ImageIcon(url)) {

            @Override
            public float getAlignmentY() {
                return 0.5f;
            }
        };

//        b.setRequestFocusEnabled(false);


        francis;

        b.setMargin();
//
//        b.setMargin(new Insets(1, 1, 1, 1));
//
//        String astr = getProperty(key + actionSuffix);
//        if (astr == null) {
//            astr = key;
//        }
//        Action a = getAction(astr);
//        if (a != null) {
//            b.setActionCommand(astr);
//            b.addActionListener(a);
//        } else {
//            b.setEnabled(false);
//        }
//
//        String tip = getResourceString(key + tipSuffix);
//        if (tip != null) {
//            b.setToolTipText(tip);
//        }
//
//        return b;
//    }
|]
