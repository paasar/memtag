module FileUtil where

import System.Directory
import System.IO
import Text.JSON
import Text.JSON.Generic

-- Own modules
import Domain

----------------------------------
-- File handling helpers
----------------------------------
readBook :: String -> IO (Book, Handle)
readBook filepath = do
  inFile <- openFile filepath ReadMode
  contents <- hGetContents inFile
  return (decodeJSON contents :: Book, inFile)

writeBook :: String -> Handle -> Book -> IO ()
writeBook filepath inFile book = do
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle $ encodeJSON book
  hClose inFile
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath