{-# LANGUAGE DeriveDataTypeable #-}
{- TODO:
    pretty print results
    delete value
    add tag
    remove tag
    list tags
    modify value
    pretty print JSON
    output options:
       sort
       show tags
    special tags (date)
    check argument amount
    tag wildcards
    tag1+tag2-tag3
    find value by part of value
  DONE:
    find value by tags
    add value
    date to items (done as a automatic tag)
-}
import qualified Control.Exception as C
import Data.List(intercalate)
import Data.Time
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
  putStrLn $ intercalate "\n" $ map value $ itemsByTags tags book
  
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
  dateTag <- createDateTag
  let updatedBook = addValueToBook value tags dateTag book
  hPutStr tempHandle $ (encodeJSON updatedBook)
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath
  putStrLn $ "Value (" ++ value ++ ") added."
addValue _ = error "Syntax: <path to file> add value tag1[:tag2...]"

addValueToBook :: String -> [Tag] -> Tag -> Book -> Book
addValueToBook value tags dateTag oldBook = oldBook {items = (createItem value tags) : (items oldBook)}
  where createItem value tags = Item {value = value, tags = dateTag : tags}

createDateTag :: IO Tag
createDateTag = do
  c <- getCurrentTime
  return ((show $ utctDay c) :: Tag)

removeByValue :: [String] -> IO ()
removeByValue [filepath, value] = do
  putStrLn "Not yet implemented."
