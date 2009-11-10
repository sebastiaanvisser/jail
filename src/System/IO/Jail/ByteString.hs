module System.IO.Jail.ByteString
( packCString
, packCStringLen
, useAsCString
, useAsCStringLen
, getLine
, getContents
, putStr
, putStrLn
, interact
, readFile
, writeFile
, appendFile
, hGetLine
, hGetContents
, hGet
, hGetNonBlocking
, hPut
, hPutStr
, hPutStrLn
)
where

import Prelude hiding (IO, getLine, getContents, putStr, putStrLn, interact, readFile, writeFile, appendFile)
import System.IO.Jail.Unsafe
import Foreign.C.String
import System.IO (Handle)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

packCString :: CString -> IO ByteString
packCString c = io (B.packCString c)

packCStringLen :: CStringLen -> IO ByteString
packCStringLen c = io (B.packCStringLen c)

useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString b f =
  do r <- mkCallback
     io (B.useAsCString b (r . f))

useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen b f =
  do r <- mkCallback
     io (B.useAsCStringLen b (r . f))

getLine :: IO ByteString
getLine = io B.getLine

getContents :: IO ByteString
getContents = io B.getContents

putStr :: ByteString -> IO ()
putStr b = io (B.putStr b)

putStrLn :: ByteString -> IO ()
putStrLn b = io (B.putStrLn b)

interact :: (ByteString -> ByteString) -> IO ()
interact f = io (B.interact f)

readFile :: FilePath -> IO ByteString
readFile = embedPath "readFile" B.readFile 

writeFile :: FilePath -> ByteString -> IO ()
writeFile f b = embedPath "writeFile" (flip B.writeFile b) f

appendFile :: FilePath -> ByteString -> IO ()
appendFile f b = embedPath "appendFile" (flip B.appendFile b) f

hGetLine :: Handle -> IO ByteString
hGetLine = embedHandle "hGetLine" B.hGetLine 

hGetContents :: Handle -> IO ByteString
hGetContents = embedHandle "hGetContents" B.hGetContents 

hGet :: Handle -> Int -> IO ByteString
hGet h i = embedHandle "hGet" (flip B.hGet i) h

hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking h i = embedHandle "hGetNonBlocking" (flip B.hGetNonBlocking i) h

hPut :: Handle -> ByteString -> IO ()
hPut h b = embedHandle "hPut" (flip B.hPut b) h

hPutStr :: Handle -> ByteString -> IO ()
hPutStr h b = embedHandle "hPutStr" (flip B.hPutStr b) h

hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h b = embedHandle "hPutStrLn" (flip B.hPutStrLn b) h

