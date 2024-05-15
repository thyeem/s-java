{-# LANGUAGE QuasiQuotes #-}

module Code.SS.Internal where

import Code.SS.Parser (Identifier)
import qualified Data.Map as M
import Data.String.Here

-- | Scope tracker
data Tracker = Tracker
  { t'path :: !String
  , t'level :: !Int
  , t'lamp :: M.Map String Tracker -- local map: key(iden-string)
  , t'gmap :: M.Map String Tracker -- global map: key(iden-string)
  , t'violations :: [Violation] -- List of violations
  }
  deriving (Show)

-- | Violation
data Violation
  = Violation (String, Identifier) (String, Identifier) -- pair of (path, iden)
  deriving (Show)

ff :: String
ff =
  [here|
		switch (type) {
			case DOCKER -> new ArrayList<>(this.dockerCommands.get(type));
			case DOCKER_COMPOSE -> {
				List<String> result = new ArrayList<>(this.dockerCommands.get(type));
				if (this.composeFile != null) {
					result.add("--file");
					result.add(this.composeFile.toString());
				}
				result.add("--ansi");
				result.add("never");
				for (String profile : this.activeProfiles) {
					result.add("--profile");
					result.add(profile);
				}
				yield result;
			}
		};

|]
