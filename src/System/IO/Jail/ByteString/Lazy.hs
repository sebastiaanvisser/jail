module System.IO.Jail.ByteString.Lazy
( getContents
, putStr
, putStrLn
, interact
, readFile
, writeFile
, appendFile
, hGetContents
, hGet
, hGetNonBlocking
, hPut
, hPutStr
)
where

import Prelude hiding (IO, getLine, getContents, putStr, putStrLn, interact, readFile, writeFile, appendFile)
import System.IO.Jail.Unsafe
import System.IO (Handle)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

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

