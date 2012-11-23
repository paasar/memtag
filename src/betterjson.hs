{-# LANGUAGE DeriveDataTypeable #-}
{- TODO:
    modify value
    delete value
    add tag
    remove tag
    pretty print
      output options:
        sort
        show tags
    special tags (date)
    check argument amount
    date to items
  -}
import qualified Control.Exception as C
import System.Environment(getArgs)
import System.Directory
import System.IO
import System.IO.Error
import Text.JSON
import Text.JSON.Generic
import Text.Regex(splitRegex, mkRegex)

data Book = Book {
    bookId :: Int
  , items :: [Item]
  } deriving (Eq, Show, Data, Typeable)

data Item = Item {
    value :: String
  , tags :: [Tag]
  } deriving (Eq, Show, Data, Typeable)

type Tag = String

---------------------------------------
syntaxMsg = "Syntax: <path to file> [add value/find] tag1[:tag2...]"


dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", addValue)  
            , ("find", findByTags)  
            , ("del", removeByValue)  
            ]


main = execute `C.catch` handler

execute :: IO ()
execute = do
  (filename:command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action (filename : args)

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e = putStrLn "The file doesn't exist!"
  | otherwise = ioError e



----------------------------------
stringToTags :: String -> [Tag]
stringToTags tagStr = splitRegex (mkRegex ":") tagStr

-- find RAAH
findByTags :: [String] -> IO ()
findByTags [filepath, tagStr] = do
  let tags = (stringToTags tagStr)
  inFile <- openFile filepath ReadMode
  contents <- hGetContents inFile
  let book = (decodeJSON contents :: Book)
  putStrLn $ show $ map value $ itemsByTags tags book
  
  hClose inFile
findByTags _ = error "Syntax: <path to file> find tag1[:tag2...]"
  
itemsByTags :: [Tag] -> Book -> [Item]
itemsByTags [] _ = []
itemsByTags (t:ts) b =
  filter (\item -> t `elem` (tags item)) (items b) ++ (itemsByTags ts b)

-- add "val1" tag1:tag2
addValue :: [String] -> IO ()
addValue [filepath, value, tagStr] = do
  let tags = stringToTags tagStr
  
  handle <- openFile filepath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  
  let book = (decodeJSON contents :: Book)
  let updatedBook = addValueToBook value tags book
  hPutStr tempHandle $ (encodeJSON updatedBook)
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath
  putStrLn $ "Value (" ++ value ++ ") added."
addValue _ = error "Syntax: <path to file> add value tag1[:tag2...]"

addValueToBook :: String -> [Tag] -> Book -> Book
addValueToBook value tags oldBook = oldBook {items = (createItem value tags) : (items oldBook)}
  where createItem value tags = Item {value = value, tags = tags}

removeByValue :: [String] -> IO ()
removeByValue [filepath, value] = do
  putStrLn "Not yet implemented."
