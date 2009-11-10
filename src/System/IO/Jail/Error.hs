module System.IO.Jail.Error
( ioError
, catch
, try
, modifyIOError
)
where

import Prelude hiding (IO, ioError, catch)
import System.IO.Jail.Unsafe
import qualified System.IO.Error as E

ioError :: IOError -> IO a
ioError = io . E.ioError

catch :: IO a -> (IOError -> IO a) -> IO a
catch a c =
  do r <- mkCallback
     io (E.catch (r a) (r . c))

try :: IO a -> IO (Either IOError a)
try a = 
  do r <- mkCallback
     io (E.try (r a))

modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError f a =
  do r <- mkCallback
     io (E.modifyIOError f (r a))

