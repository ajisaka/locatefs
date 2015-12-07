
{-# LANGUAGE  TupleSections, ScopedTypeVariables #-}

module Main where

import Control.Applicative
import qualified Control.Concurrent.SSem as Sem
import Control.Exception
import Control.Monad (forM)
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as H
import System.Environment (getArgs)
import System.Fuse
import System.IO (hPutStr, hGetLine, hClose, hGetContents)
import System.Posix.Files
import System.Posix.Syslog
import System.Posix.Types (ByteCount, FileOffset, FileMode(..))
import System.Process (createProcess, proc, StdStream(..), CreateProcess(..))



-- main

main :: IO ()
main = withSyslog "locatefs" [PID, PERROR] USER (logUpTo Debug) $ do
    cache <- H.new :: IO Cache
    sem <- Sem.new 1
    args <- getArgs
    p "Starting locatefs" args
    fuseMain (lfsOps sem cache) exceptionHandler
  where
    exceptionHandler e = p "Meow" e >> defaultExceptionHandler e



-- types

type Unit = ()

type Entries = [String]

type Cache = H.BasicHashTable String Entries

type Semaphore = Sem.SSem



-- debug

p :: Show a => String -> a -> IO ()
p name x = syslog Debug $ name ++ ": " ++ show x



-- cache

fetch :: Semaphore -> Cache -> String -> IO Entries
fetch sem cache query = Sem.withSem sem $ do
    found <- H.lookup cache query
    case found of
         Just x -> return x
         Nothing -> do
           entries <- locate query
           H.insert cache query entries
           return entries



-- funcs

isSep :: Char -> Bool
isSep = (== '/')

-- "/" → ("", "")
-- "/foo" → ("foo", "")
-- "/foo/" → ("foo", "")
-- "/foo/" → ("foo", "/")
-- "/foo/bar" → ("foo", "/bar")
-- "///foo/bar" → ("foo", "/bar")
extractQuery :: String -> (String, String)
extractQuery x = break isSep $ dropWhile isSep x


procPath :: a -> (String -> a) -> (String -> a) -> String -> a
procPath r q p x = case extractQuery x of
                        ("", "")      -> r
                        (query, "")   -> q query
                        (query, path) -> p path


commandResult :: String -> [String] -> IO [String]
commandResult cmd args = createProcess (proc cmd args) { std_out = CreatePipe } >>= pi
  where
    pi (_, Just hOut, _, _) = lines <$> hGetContents hOut
    pi _                    = error "Unable to start process"


locate :: String -> IO [String]
locate = commandResult "locate" . (["--existing", "--follow"] ++) . return


escape :: FilePath -> FilePath
escape = f . dropWhile isSep
  where
    f []       = []
    f ('/':xs) = "_-" ++ f xs
    f ('_':xs) = "__" ++ f xs
    f (x:xs)   = x : f xs


unescape :: FilePath -> FilePath
unescape []       = ""
unescape ('_':'_':xs) = "_" ++ unescape xs
unescape ('_':'-':xs) = "/" ++ unescape xs
unescape (x:xs)   = x : unescape xs


escapeTest query = do
    fs <- locate query
    return $ filter isBad fs
  where
    isBad x = let l = (unescape $ escape x) /= (dropWhile isSep x)
                  r = dropWhile isSep x /= ""
              in l && r


-- work around
fixString :: String -> String
fixString = B.unpack . B8.fromString



-- stat

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat { statEntryType        = Directory
                       , statFileMode         = foldr1 unionFileModes modes
                       , statLinkCount        = 2
                       , statFileOwner        = fuseCtxUserID ctx
                       , statFileGroup        = fuseCtxGroupID ctx
                       , statSpecialDeviceID  = 0
                       , statFileSize         = 4096
                       , statBlocks           = 1
                       , statAccessTime       = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0 }
  where
    modes = [ownerReadMode, groupReadMode]


linkStat :: FilePath -> IO FileStat
linkStat path = stat <$> getFileStatus path
  where
    stat status = FileStat { statEntryType        = SymbolicLink
                           , statFileMode         = fileMode status
                           , statLinkCount        = linkCount status
                           , statFileOwner        = fileOwner status
                           , statFileGroup        = fileGroup status
                           , statSpecialDeviceID  = specialDeviceID status
                           , statFileSize         = fileSize status
                           , statBlocks           = 1 -- fromIntegral (fileSize status `div` 1024)
                           , statAccessTime       = accessTime status
                           , statModificationTime = modificationTime status
                           , statStatusChangeTime = statusChangeTime status }



-- fuse

lfsOps :: Semaphore -> Cache -> FuseOperations Unit
lfsOps sem cache = defaultFuseOps { fuseGetFileStat = lfsGetFileStat
                                  , fuseOpenDirectory = lfsOpenDirectory
                                  , fuseReadDirectory = lfsReadDirectory sem cache
                                  , fuseReadSymbolicLink = lfsReadSymbolicLink
                                  , fuseGetFileSystemStats = lfsGetFileSystemStats }


lfsGetFileStat :: FilePath -> IO (Either Errno FileStat)
lfsGetFileStat = procPath r (const r) qp . unescape
  where
    r = Right . dirStat <$> getFuseContext
    qp fp = do exist <- fileExist fp
               if exist
               then Right <$> linkStat fp
               else return $ Left eNOENT


lfsOpenDirectory :: FilePath -> IO Errno
lfsOpenDirectory _ = return eOK


lfsReadDirectory :: Semaphore -> Cache -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
lfsReadDirectory sem cache = procPath r q qp
  where
    r = Right . dots <$> getFuseContext
    q query = do
        ctx <- getFuseContext
        ls <- fetch sem cache query
        Right <$> forM ls entry
    qp _ = return $ Left eNOENT
    dots ctx = [(".", dirStat ctx), ("..", dirStat ctx)]
    entry path = (escape path,) <$> linkStat path


lfsReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)
lfsReadSymbolicLink = return . procPath r (const r) qp . unescape
  where
    r = Left eNOENT
    qp = Right . fixString


lfsGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
lfsGetFileSystemStats _ =
    return $ Right $ FileSystemStats
      { fsStatBlockSize = 512
      , fsStatBlockCount = 1
      , fsStatBlocksFree = 1
      , fsStatBlocksAvailable = 1
      , fsStatFileCount = 5
      , fsStatFilesFree = 10
      , fsStatMaxNameLength = 4096 }
