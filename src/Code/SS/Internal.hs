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
public abstract class CacheRepository<T> {

    abstract String key(T entity);

    abstract Class<T> getCacheClass();

    String value(T entity) {
        return gson.toJson(entity);
    }

    abstract LocalDateTime expireAt(T entity);

    public Optional<T> getCacheData(String key) {
        return get(key)
        .map(v -> {
                    log.debug("cache data - key: {}, value: {}", key, v);
                    return v;
                }).map(v -> gson.fromJson(v, getCacheClass()));
    }
}
|]
