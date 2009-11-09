{-# LANGUAGE CPP #-}
module System.IO.MonadJail where

import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Foreign.Ptr
import Prelude hiding (readFile, writeFile, print, appendFile, IO, getChar, getLine, getContents, readIO, readLn, interact, putChar, putStr, putStrLn)
import System.IO (IOMode, Handle, BufferMode, HandlePosn, SeekMode)
import System.IO.Jail (IO)
import qualified System.IO.Jail as Jail

-- | Like `MonadIO`, but for jailed computations.

class Monad m => MonadJail m where
  jailIO :: IO a -> m a

instance MonadJail IO where
  jailIO = id

instance            MonadJail m  => MonadJail (ContT     r     m) where jailIO = lift . jailIO
instance (Error e,  MonadJail m) => MonadJail (ErrorT    e     m) where jailIO = lift . jailIO
instance            MonadJail m  => MonadJail (IdentityT       m) where jailIO = lift . jailIO
instance            MonadJail m  => MonadJail (ListT           m) where jailIO = lift . jailIO
instance (Monoid w, MonadJail m) => MonadJail (RWST      r w s m) where jailIO = lift . jailIO
instance            MonadJail m  => MonadJail (ReaderT   r     m) where jailIO = lift . jailIO
instance            MonadJail m  => MonadJail (StateT    r     m) where jailIO = lift . jailIO
instance (Monoid r, MonadJail m) => MonadJail (WriterT   r     m) where jailIO = lift . jailIO

-- Embedded IO actions.

withFile :: MonadJail m => FilePath -> IOMode -> (Handle -> IO a) -> m a
withFile f m w = jailIO (Jail.withFile f m w)

openFile :: MonadJail m => FilePath -> IOMode -> m Handle
openFile f m = jailIO (Jail.openFile f m)

hClose :: MonadJail m => Handle -> m ()
hClose h = jailIO (Jail.hClose h)

--

readFile :: MonadJail m => FilePath -> m String
readFile f = jailIO (Jail.readFile f)

writeFile :: MonadJail m => FilePath -> String -> m ()
writeFile f s = jailIO (Jail.writeFile f s)

appendFile :: MonadJail m => FilePath -> String -> m ()
appendFile f s = jailIO (Jail.appendFile f s)

--

hFileSize :: MonadJail m => Handle -> m Integer
hFileSize h = jailIO (Jail.hFileSize h)

#ifdef __GLASGOW_HASKELL__

hSetFileSize :: MonadJail m => Handle -> Integer -> m ()
hSetFileSize h i = jailIO (Jail.hSetFileSize h i)

#endif

--

hIsEOF :: MonadJail m => Handle -> m Bool
hIsEOF h = jailIO (Jail.hIsEOF h)

isEOF :: MonadJail m => m Bool
isEOF = jailIO Jail.isEOF

--

hSetBuffering :: MonadJail m => Handle -> BufferMode -> m ()
hSetBuffering h m = jailIO (Jail.hSetBuffering h m)

hGetBuffering :: MonadJail m => Handle -> m BufferMode
hGetBuffering h = jailIO (Jail.hGetBuffering h)

hFlush :: MonadJail m => Handle -> m ()
hFlush h = jailIO (Jail.hFlush h)

--

hGetPosn :: MonadJail m => Handle -> m HandlePosn
hGetPosn h = jailIO (Jail.hGetPosn h)

hSetPosn :: MonadJail m => HandlePosn -> m ()
hSetPosn h = jailIO (Jail.hSetPosn h)

hSeek :: MonadJail m => Handle -> SeekMode -> Integer -> m ()
hSeek h m i = jailIO (Jail.hSeek h m i)

#if !defined(__NHC__)

hTell :: MonadJail m => Handle -> m Integer
hTell h = jailIO (Jail.hTell h)

#endif

--

hIsOpen :: MonadJail m => Handle -> m Bool
hIsOpen h = jailIO (Jail.hIsOpen h)

hIsClosed :: MonadJail m => Handle -> m Bool
hIsClosed h = jailIO (Jail.hIsClosed h)

hIsReadable :: MonadJail m => Handle -> m Bool
hIsReadable h = jailIO (Jail.hIsReadable h)

hIsWritable :: MonadJail m => Handle -> m Bool
hIsWritable h = jailIO (Jail.hIsWritable h)

hIsSeekable :: MonadJail m => Handle -> m Bool
hIsSeekable h = jailIO (Jail.hIsSeekable h)

--

#if !defined(__NHC__)

hIsTerminalDevice :: MonadJail m => Handle -> m Bool
hIsTerminalDevice h = jailIO (Jail.hIsTerminalDevice h)

hSetEcho :: MonadJail m => Handle -> Bool -> m ()
hSetEcho h e = jailIO (Jail.hSetEcho h e)

hGetEcho :: MonadJail m => Handle -> m Bool
hGetEcho h = jailIO (Jail.hGetEcho h)

#endif

--

#ifdef __GLASGOW_HASKELL__

hShow :: MonadJail m => Handle -> m String
hShow h = jailIO (Jail.hShow h)

#endif

--

hWaitForInput :: MonadJail m => Handle -> Int -> m Bool
hWaitForInput h i = jailIO (Jail.hWaitForInput h i)

hReady :: MonadJail m => Handle -> m Bool
hReady h = jailIO (Jail.hReady h)

hGetChar :: MonadJail m => Handle -> m Char
hGetChar h = jailIO (Jail.hGetChar h)

hGetLine :: MonadJail m => Handle -> m String
hGetLine h = jailIO (Jail.hGetLine h)

hLookAhead :: MonadJail m => Handle -> m Char
hLookAhead h = jailIO (Jail.hLookAhead h)

hGetContents :: MonadJail m => Handle -> m String
hGetContents h = jailIO (Jail.hGetContents h)

--

hPutChar :: MonadJail m => Handle -> Char -> m ()
hPutChar h c = jailIO (Jail.hPutChar h c)

hPutStr :: MonadJail m => Handle -> String -> m ()
hPutStr h s = jailIO (Jail.hPutStr h s)

hPutStrLn :: MonadJail m => Handle -> String -> m ()
hPutStrLn h s = jailIO (Jail.hPutStrLn h s)

hPrint :: (MonadJail m, Show a) => Handle -> a -> m ()
hPrint h s = jailIO (Jail.hPrint h s)

--

interact :: MonadJail m => (String -> String) -> m ()
interact a = jailIO (Jail.interact a)

putChar :: MonadJail m => Char -> m ()
putChar a = jailIO (Jail.putChar a)

putStr :: MonadJail m => String -> m ()
putStr a = jailIO (Jail.putStr a)

putStrLn :: MonadJail m => String -> m ()
putStrLn a = jailIO (Jail.putStrLn a)

print :: (MonadJail m, Show a) => a -> m ()
print a = jailIO (Jail.print a)

--

getChar :: MonadJail m => m Char
getChar = jailIO Jail.getChar

getLine :: MonadJail m => m String
getLine = jailIO Jail.getLine

getContents :: MonadJail m => m String
getContents = jailIO Jail.getContents

readIO :: (MonadJail m, Read a) => String -> m a
readIO s = jailIO (Jail.readIO s)

readLn :: (MonadJail m, Read a) => m a
readLn = jailIO Jail.readLn

--

withBinaryFile :: MonadJail m => FilePath -> IOMode -> (Handle -> IO a) -> m a
withBinaryFile f m w = jailIO (Jail.withBinaryFile f m w)

openBinaryFile :: MonadJail m => FilePath -> IOMode -> m Handle
openBinaryFile f m = jailIO (Jail.openBinaryFile f m)

hSetBinaryMode :: MonadJail m => Handle -> Bool -> m ()
hSetBinaryMode h b = jailIO (Jail.hSetBinaryMode h b)

hPutBuf :: MonadJail m => Handle -> Ptr a -> Int -> m ()
hPutBuf h p i = jailIO (Jail.hPutBuf h p i)

hGetBuf :: MonadJail m => Handle -> Ptr a -> Int -> m Int
hGetBuf h p i = jailIO (Jail.hGetBuf h p i)

#if !defined(__NHC__) && !defined(__HUGS__)

hPutBufNonBlocking :: MonadJail m => Handle -> Ptr a -> Int -> m Int
hPutBufNonBlocking h p i = jailIO (Jail.hPutBufNonBlocking h p i)

hGetBufNonBlocking :: MonadJail m => Handle -> Ptr a -> Int -> m Int
hGetBufNonBlocking h p i = jailIO (Jail.hGetBufNonBlocking h p i)

#endif

--

openTempFile :: MonadJail m => FilePath -> String -> m (FilePath, Handle)  
openTempFile f s = jailIO (Jail.openTempFile f s)

openBinaryTempFile :: MonadJail m => FilePath -> String -> m (FilePath, Handle) 
openBinaryTempFile f s = jailIO (Jail.openBinaryTempFile f s)

