{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module System.IO.Jail.Unsafe where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Set (Set)
import Data.Typeable
import Prelude hiding (IO)
import System.IO (Handle)
import qualified Data.Set as Set
import qualified System.Directory as D
import qualified System.IO as U

-- Make `Handle's orderable.

data HandleS = HandleS String Handle

mkHWrap :: Handle -> HandleS
mkHWrap h = HandleS (show h) h

instance Eq HandleS where
  (HandleS _ s) == (HandleS _ t) = s == t

instance Ord HandleS where
  (HandleS s _) `compare` (HandleS t _) = s `compare` t

-- | The jailed IO monad.

newtype IO a = IO { unJail :: ReaderT (Maybe FilePath) (StateT (Set HandleS) U.IO) a}
  deriving (Functor, Applicative, Monad, Typeable, MonadFix)

{- |
Run a jailed IO computation. The IO computation will be able to access all
files that are within the specified jail directory. All file accesses outside
the jail directory will be refused. Only file handles opened from within the
jailed computation and the handles from the white list will be accessible to
the operations requiring a file handle. No smuggling in of foreign handles,
border patrol is very strict. When the jail path is specified as `Nothing' no
file access will be possible at all, this means the computation can only rely
on the white listed handles.
-}

run
  :: Maybe FilePath  -- ^ The jail directory or `Nothing' for not allowing file access.
  -> [Handle]        -- ^ A white list of handles that are always accessible.
  -> IO a            -- ^ The jailed IO computation to run.
  -> U.IO a          -- ^ Run the computation from within the insecure real world.
run jail = runRaw jail . Set.fromList . map mkHWrap

runRaw :: Maybe FilePath -> Set HandleS -> IO a -> U.IO a
runRaw p h =
    flip evalStateT h
  . flip runReaderT p
  . unJail

isSubPathOf :: FilePath -> FilePath -> U.IO Bool
isSubPathOf path jail = isPrefixOf <$> D.canonicalizePath jail <*> D.canonicalizePath path

-- Create 

mkCallback :: IO (IO a -> U.IO a)
mkCallback = runRaw <$> IO ask <*> IO (lift get) 

-- Unconditionally, embed an IO action into the Jail monad. Not to be exported!

io :: U.IO a -> IO a
io = IO . liftIO

-- Allow a new Handle.

allow :: Handle -> IO ()
allow h = (IO . lift) (modify (Set.insert (mkHWrap h)))

{-
Embed an IO action that takes a FilePath as input. The IO action will only
executed when the path is within the jail directory. Not to be exported!
-}

embedPaths :: String -> ([FilePath] -> U.IO a) -> [FilePath] -> IO a
embedPaths name action paths = 
  do jail <- IO ask 
     safe <- mapM (\p -> io (maybe (return False) (p `isSubPathOf`) jail)) paths
     if and safe
       then io (action paths)
       else error (name ++ ": Permission denied, filepath outside jailed environment. "
                  ++ show paths)

embedPath :: String -> (FilePath -> U.IO a) -> FilePath -> IO a
embedPath name action path = embedPaths name (action . head) [path]

{-
Embed an IO action that takes a `Handle' as input. The IO action will only
executed when the handle is opened from within the jailed IO monad. Not to be
exported!
-}

embedHandles :: String -> ([Handle] -> U.IO a) -> [Handle] -> IO a
embedHandles name action handles = 
  do set <- IO (lift get)
     if all (\h -> mkHWrap h `Set.member` set) handles
       then io (action handles)
       else error (name ++ ": Permission denied, handle from outside jailed environment. "
                  ++ show handles)

embedHandle :: String -> (Handle -> U.IO a) -> Handle -> IO a
embedHandle name action handle = embedHandles name (action . head) [handle]

