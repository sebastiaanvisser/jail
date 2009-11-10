module System.IO.Jail.Timeout (timeout) where

import Prelude hiding (IO)
import System.IO.Jail.Unsafe
import qualified System.Timeout as T

timeout :: Int -> IO a -> IO (Maybe a)
timeout i a =
  do r <- mkCallback
     io (T.timeout i (r a))

