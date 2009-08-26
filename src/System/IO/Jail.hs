{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module System.IO.Jail
  ( -- * The IO monad

    IO,                        -- instance MonadFix
    run,
    JailIO (..),

    -- * Files and handles

    FilePath,                  -- :: String

    Handle,             -- abstract, instance of: Eq, Show.

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

    stdin, stdout, stderr,     -- :: Handle

    -- * Opening and closing files

    -- ** Opening files

    withFile,
    openFile,                  -- :: FilePath -> IOMode -> IO Handle
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

    hClose,                    -- :: Handle -> IO ()

    -- ** Special cases

    readFile,                  -- :: FilePath -> IO String
    writeFile,                 -- :: FilePath -> String -> IO ()
    appendFile,                -- :: FilePath -> String -> IO ()

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining and changing the size of a file

    hFileSize,                 -- :: Handle -> IO Integer

#ifdef __GLASGOW_HASKELL__
    hSetFileSize,              -- :: Handle -> Integer -> IO ()
#endif

    -- ** Detecting the end of input

    hIsEOF,                    -- :: Handle -> IO Bool
    isEOF,                     -- :: IO Bool

    -- ** Buffering operations

    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    hSetBuffering,             -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,             -- :: Handle -> IO BufferMode
    hFlush,                    -- :: Handle -> IO ()

    -- ** Repositioning handles

    hGetPosn,                  -- :: Handle -> IO HandlePosn
    hSetPosn,                  -- :: HandlePosn -> IO ()
    HandlePosn,                -- abstract, instance of: Eq, Show.

    hSeek,                     -- :: Handle -> SeekMode -> Integer -> IO ()
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),

#if !defined(__NHC__)
    hTell,                     -- :: Handle -> IO Integer
#endif

    -- ** Handle properties

    hIsOpen, hIsClosed,        -- :: Handle -> IO Bool
    hIsReadable, hIsWritable,  -- :: Handle -> IO Bool
    hIsSeekable,               -- :: Handle -> IO Bool

    -- ** Terminal operations (not portable: GHC\/Hugs only)

#if !defined(__NHC__)
    hIsTerminalDevice,          -- :: Handle -> IO Bool

    hSetEcho,                   -- :: Handle -> Bool -> IO ()
    hGetEcho,                   -- :: Handle -> IO Bool
#endif

    -- ** Showing handle state (not portable: GHC only)

#ifdef __GLASGOW_HASKELL__
    hShow,                      -- :: Handle -> IO String
#endif

    -- * Text input and output

    -- ** Text input

    hWaitForInput,             -- :: Handle -> Int -> IO Bool
    hReady,                    -- :: Handle -> IO Bool
    hGetChar,                  -- :: Handle -> IO Char
    hGetLine,                  -- :: Handle -> IO [Char]
    hLookAhead,                -- :: Handle -> IO Char
    hGetContents,              -- :: Handle -> IO [Char]

    -- ** Text output

    hPutChar,                  -- :: Handle -> Char -> IO ()
    hPutStr,                   -- :: Handle -> [Char] -> IO ()
    hPutStrLn,                 -- :: Handle -> [Char] -> IO ()
    hPrint,                    -- :: Show a => Handle -> a -> IO ()

    -- ** Special cases for standard input and output

    interact,                  -- :: (String -> String) -> IO ()
    putChar,                   -- :: Char   -> IO ()
    putStr,                    -- :: String -> IO () 
    putStrLn,                  -- :: String -> IO ()
    print,                     -- :: Show a => a -> IO ()
    getChar,                   -- :: IO Char
    getLine,                   -- :: IO String
    getContents,               -- :: IO String
    readIO,                    -- :: Read a => String -> IO a
    readLn,                    -- :: Read a => IO a

    -- * Binary input and output

    withBinaryFile,
    openBinaryFile,            -- :: FilePath -> IOMode -> IO Handle
    hSetBinaryMode,            -- :: Handle -> Bool -> IO ()
    hPutBuf,                   -- :: Handle -> Ptr a -> Int -> IO ()
    hGetBuf,                   -- :: Handle -> Ptr a -> Int -> IO Int

#if !defined(__NHC__) && !defined(__HUGS__)
    hPutBufNonBlocking,        -- :: Handle -> Ptr a -> Int -> IO Int
    hGetBufNonBlocking,        -- :: Handle -> Ptr a -> Int -> IO Int
#endif

    -- * Temporary files

    openTempFile,
    openBinaryTempFile,
  )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Set (Set)
import Data.Typeable
import Foreign.Ptr
import Prelude hiding (readFile, writeFile, print, appendFile, IO, getChar, getLine, getContents, readIO, readLn, interact, putChar, putStr, putStrLn)
import System.Directory
import System.IO (IOMode, Handle, BufferMode, HandlePosn, SeekMode, stdin, stdout, stderr)
import qualified Data.Set as Set
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

-- | Like `MoandIO`, but for jailed computations.

class Monad m => JailIO m where
  jailIO :: IO a -> m a

instance JailIO IO where
  jailIO = id

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
isSubPathOf path jail = isPrefixOf <$> canonicalizePath jail <*> canonicalizePath path

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

embedPath :: String -> (FilePath -> U.IO a) -> FilePath -> IO a
embedPath name action path = 
  do jail <- IO ask 
     safe <- io (maybe (return False) (path `isSubPathOf`) jail)
     if safe
       then io (action path)
       else error (name ++ ": Permission denied, filepath outside jailed environment. "
                  ++ show path)

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

-- Embedded IO actions.

withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile f m w =
  do r <- runRaw <$> IO ask <*> IO (lift get)
     embedPath "withFile" (\f' -> U.withFile f' m (\h -> r (allow h >> w h))) f

openFile :: FilePath -> IOMode -> IO Handle
openFile f m =
  do h <- embedPath "openFile" (flip U.openFile m) f
     allow h
     return h

hClose :: Handle -> IO ()
hClose = io . U.hClose

--

readFile :: FilePath -> IO String
readFile = embedPath "readFile" U.readFile

writeFile :: FilePath -> String -> IO ()
writeFile f s = embedPath "writeFile" (flip U.writeFile s) f

appendFile :: FilePath -> String -> IO ()
appendFile f s = embedPath "appendFile" (flip U.appendFile s) f

--

hFileSize :: Handle -> IO Integer
hFileSize = embedHandle "hFileSize" U.hFileSize

#ifdef __GLASGOW_HASKELL__

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize h i = embedHandle "setFileSize" (flip U.hSetFileSize i) h

#endif

--

hIsEOF :: Handle -> IO Bool
hIsEOF = io . U.hIsEOF

isEOF :: IO Bool
isEOF = io U.isEOF

--

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering h m = embedHandle "hSetBuffering" (flip U.hSetBuffering m) h

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering = embedHandle "hGetBuffering" U.hGetBuffering

hFlush :: Handle -> IO ()
hFlush = embedHandle "hFlush" U.hFlush

--

hGetPosn :: Handle -> IO HandlePosn
hGetPosn = embedHandle "hGetPosn" U.hGetPosn

hSetPosn :: HandlePosn -> IO ()
hSetPosn = io . U.hSetPosn

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h m i = embedHandle "hSeek" (\h' -> U.hSeek h' m i) h

#if !defined(__NHC__)

hTell :: Handle -> IO Integer
hTell = embedHandle "hTell" U.hTell

#endif

--

hIsOpen :: Handle -> IO Bool
hIsOpen = embedHandle "hIsOpen" U.hIsOpen

hIsClosed :: Handle -> IO Bool
hIsClosed = embedHandle "hIsClosed" U.hIsClosed

hIsReadable :: Handle -> IO Bool
hIsReadable = embedHandle "hIsReadable" U.hIsReadable

hIsWritable :: Handle -> IO Bool
hIsWritable = embedHandle "hIsWritable" U.hIsWritable

hIsSeekable :: Handle -> IO Bool
hIsSeekable = embedHandle "hIsSeekable" U.hIsSeekable

--

#if !defined(__NHC__)

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice = embedHandle "hIsTerminalDevice" U.hIsTerminalDevice

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho h e = embedHandle "hSetEcho" (flip U.hSetEcho e) h

hGetEcho :: Handle -> IO Bool
hGetEcho = embedHandle "hGetEcho" U.hGetEcho

#endif

--

#ifdef __GLASGOW_HASKELL__

hShow :: Handle -> IO String
hShow = embedHandle "hShow" U.hShow

#endif

--

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput h i = embedHandle "hWaitForInput" (flip U.hWaitForInput i) h

hReady :: Handle -> IO Bool
hReady = embedHandle "hReady" U.hReady

hGetChar :: Handle -> IO Char
hGetChar = embedHandle "hGetChar" U.hGetChar

hGetLine :: Handle -> IO String
hGetLine = embedHandle "hGetLine" U.hGetLine

hLookAhead :: Handle -> IO Char
hLookAhead = embedHandle "hLookAhead" U.hLookAhead

hGetContents :: Handle -> IO String
hGetContents = embedHandle "hGetContents" U.hGetContents

--

hPutChar :: Handle -> Char -> IO ()
hPutChar h c = embedHandle "hPutChar" (flip U.hPutChar c) h

hPutStr :: Handle -> String -> IO ()
hPutStr h s = embedHandle "hPutStr" (flip U.hPutStr s) h

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = embedHandle "hPutStrLn" (flip U.hPutStrLn s) h

hPrint :: Show a => Handle -> a -> IO ()
hPrint h s = embedHandle "hPrint" (flip U.hPrint s) h

--

interact :: (String -> String) -> IO ()
interact a = embedHandles "interact" (const (U.interact a)) [stdout, stdin]

putChar :: Char -> IO ()
putChar a = embedHandle "putChar" (const (U.putChar a)) stdout

putStr :: String -> IO ()
putStr a = embedHandle "putStr" (const (U.putStr a)) stdout

putStrLn :: String -> IO ()
putStrLn a = embedHandle "putStrLn" (const (U.putStrLn a)) stdout

print :: Show a => a -> IO ()
print a = embedHandle "print" (const (U.print a)) stdout

--

getChar :: IO Char
getChar = embedHandle "getChar" (const U.getChar) stdin

getLine :: IO String
getLine = embedHandle "getLine" (const U.getLine) stdin

getContents :: IO String
getContents = embedHandle "getContents" (const U.getContents) stdin

readIO :: Read a => String -> IO a
readIO s = embedHandle "readIO" (const (U.readIO s)) stdin

readLn :: Read a => IO a
readLn = embedHandle "readLn" (const U.readLn) stdin

--

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFile f m w =
  do r <- runRaw <$> IO ask <*> IO (lift get)
     embedPath "withBinaryFile" (\f' -> U.withBinaryFile f' m (\h -> r (allow h >> w h))) f

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile f m =
  do h <- embedPath "openBinaryFile" (flip U.openBinaryFile m) f
     allow h
     return h

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode h b = embedHandle "hSetBinaryMode" (flip U.hSetBinaryMode b) h

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf h p i = embedHandle "hPutBuf" (\h' -> U.hPutBuf h' p i) h

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf h p i = embedHandle "hGetBuf" (\h' -> U.hGetBuf h' p i) h

#if !defined(__NHC__) && !defined(__HUGS__)

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking h p i = embedHandle "hPutBufNonBlocking" (\h' -> U.hPutBufNonBlocking h' p i) h

hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking h p i = embedHandle "hGetBufNonBlocking" (\h' -> U.hGetBufNonBlocking h' p i) h

#endif

--

openTempFile :: FilePath -> String -> IO (FilePath, Handle)  
openTempFile f s = embedPath "openTempFile" (flip U.openTempFile s) f

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle) 
openBinaryTempFile f s = embedPath "openBinaryTempFile" (flip U.openBinaryTempFile s) f

