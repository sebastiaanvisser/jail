module System.IO.Jail.Directory
( createDirectory
, createDirectoryIfMissing
, removeDirectory
, removeDirectoryRecursive
, renameDirectory
, getDirectoryContents
, getCurrentDirectory
, setCurrentDirectory
, getHomeDirectory
, getAppUserDataDirectory
, getUserDocumentsDirectory
, getTemporaryDirectory
, removeFile
, renameFile
, copyFile
, canonicalizePath
, makeRelativeToCurrentDirectory
, findExecutable
, doesFileExist
, doesDirectoryExist
, getPermissions
, setPermissions
, getModificationTime
)
where

import Prelude hiding (IO)
import System.IO.Jail.Unsafe
import System.Time (ClockTime)
import qualified System.Directory as D

createDirectory :: FilePath -> IO ()
createDirectory = embedPath "createDirectory" D.createDirectory 

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing b = embedPath "createDirectoryIfMissing" (D.createDirectoryIfMissing b)

removeDirectory :: FilePath -> IO ()
removeDirectory = embedPath "removeDirectory" D.removeDirectory 

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = embedPath "removeDirectoryRecursive" D.removeDirectoryRecursive 

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory a b = embedPaths "renameDirectory" (\[c, d] -> D.renameDirectory c d) [a, b]

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = embedPath "getDirectoryContents" D.getDirectoryContents 

getCurrentDirectory :: IO FilePath
getCurrentDirectory = io D.getCurrentDirectory 

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = embedPath "setCurrentDirectory" D.setCurrentDirectory 

getHomeDirectory :: IO FilePath
getHomeDirectory = io D.getHomeDirectory 

getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory = embedPath "getAppUserDataDirectory" D.getAppUserDataDirectory 

getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = io D.getUserDocumentsDirectory 

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = io D.getTemporaryDirectory 

removeFile :: FilePath -> IO ()
removeFile = embedPath "removeFile" D.removeFile 

renameFile :: FilePath -> FilePath -> IO ()
renameFile a b = embedPaths "renameFile" (\[f, g] -> D.renameFile f g) [a, b]

copyFile :: FilePath -> FilePath -> IO ()
copyFile a b = embedPaths "copyFile" (\[f, g] -> D.copyFile f g) [a, b]

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = embedPath "canonicalizePath" D.canonicalizePath 

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory = embedPath "makeRelativeToCurrentDirectory" D.makeRelativeToCurrentDirectory 

findExecutable :: String -> IO (Maybe FilePath)
findExecutable = io . D.findExecutable 

doesFileExist :: FilePath -> IO Bool
doesFileExist = embedPath "doesFileExist" D.doesFileExist 

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = embedPath "doesDirectoryExist" D.doesDirectoryExist 

getPermissions :: FilePath -> IO D.Permissions
getPermissions = embedPath "getPermissions" D.getPermissions 

setPermissions :: FilePath -> D.Permissions -> IO ()
setPermissions f p = embedPath "setPermissions" (flip D.setPermissions p) f

getModificationTime :: FilePath -> IO ClockTime
getModificationTime = embedPath "getModificationTime" D.getModificationTime 

