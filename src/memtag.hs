{-# LANGUAGE DeriveDataTypeable #-}
{- TODO:
    add tag,remove tag (tag value +tag2-tag1)
    delete value(s) by tag(s)
    list tags
    list all values
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
    refactor file handling into one place
  DONE:
    find value by tags
    add value
    pretty print results
    date to items (done as a automatic tag)
    delete value
    delete by id
-}
import qualified Control.Exception as C
import Data.List(intercalate,(\\))
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
    itemId :: Int
  , value :: String
  , tags :: [Tag]
  } deriving (Eq, Show, Data, Typeable)

type Tag = String

---------------------------------------
---------------------------------------
syntaxMsg = "Syntax: <path to file> find tag1[:tag2...]\n" ++
            "                       add value tag1[:tag2...]\n" ++
            "                       del value\n" ++
            "                       tag value (+/-)tag1[(+/-)tag2...]"

tooFewArgsMsg = "Too few arguments.\n" ++ syntaxMsg

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", addValue)
            , ("find", findByTags)
            , ("del", deleteByValue)
            , ("delid", deleteById)
            , ("tag", changeTags)
            ]

main = execute `C.catch` handler

execute :: IO ()
execute = do
  possibleArgs <- getArgs
  (filename:commandName:args) <- parseArgs possibleArgs
  let maybeAction = lookup commandName dispatch
  
  executeAction maybeAction commandName (filename : args)

parseArgs :: [String] -> IO [String]
parseArgs [] = fail tooFewArgsMsg
parseArgs [a] = fail tooFewArgsMsg
parseArgs args = return args

executeAction :: Maybe ([String] -> IO ()) -> String -> [String] -> IO ()
executeAction maybeAction commandName args =
  case maybeAction of
    Nothing -> fail ("Command '" ++ commandName ++ "' not found.\n" ++ syntaxMsg)
    Just action -> do action args

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e = putStrLn "The file doesn't exist!"
  | otherwise = ioError e


----------------------------------
-- find RAAH
findByTags :: [String] -> IO ()
findByTags [filepath, tagStr] = do
  let tags = (stringToTags tagStr)
  inFile <- openFile filepath ReadMode
  contents <- hGetContents inFile
  let book = (decodeJSON contents :: Book)

  putStrLn $ printResult $ itemsByTags tags book
  
  hClose inFile
findByTags _ = error "Syntax: <path to file> find tag1[:tag2...]"

printResult :: [Item] -> String
printResult [] = ""
printResult (item:rest) = show (itemId item) ++ ": " ++ value item ++ "\n"
                          ++ printResult rest

stringToTags :: String -> [Tag]
stringToTags tagStr = splitRegex (mkRegex ":") tagStr

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
  let nextId = nextItemId $ items book
  let updatedBook = addValueToBook value tags dateTag nextId book
  hPutStr tempHandle $ (encodeJSON updatedBook)
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath
  putStrLn $ "Value \"" ++ value ++ "\" (id: " ++ show nextId ++ ") added."
addValue _ = error "Syntax: <path to file> add value tag1[:tag2...]"

createDateTag :: IO Tag
createDateTag = do
  c <- getCurrentTime
  return ("Date " ++ (show $ utctDay c) :: Tag)

addValueToBook :: String -> [Tag] -> Tag -> Int -> Book -> Book
addValueToBook value tags dateTag nextId oldBook =
    oldBook {items = (createItem value tags) : oldItems}
  where createItem value tags = Item {itemId = nextId,
                                      value = value,
                                      tags = dateTag : tags}
        oldItems = (items oldBook)

nextItemId :: [Item] -> Int
nextItemId oldItems = (itemId (foldl1 maxId oldItems)) + 1
  where maxId x y = case compare (itemId x) (itemId y) of
                    GT -> x
                    _  -> y

-- del "valx"
deleteByValue :: [String] -> IO ()
deleteByValue [filepath, valueStr] = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  let book = (decodeJSON contents :: Book)
  
  let originalItems = items book
  let updatedBook = deleteFromBook value valueStr book
  let newItems = items updatedBook
  
  if originalItems == newItems
    then do
      putStrLn "No match found, nothing done."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      hPutStr tempHandle $ (encodeJSON updatedBook)
      hClose tempHandle
      
      removeFile filepath
      renameFile tempName filepath
      
      putStrLn $ "Value \"" ++ valueStr ++ "\" deleted."
  hClose handle
deleteByValue _ = error "Syntax: <path to file> del value"

-- | Delete Item by Item attribute.
-- First parameter is the function to extract attribute value.
-- Second parameter is the target value which should match.
deleteFromBook :: (Eq a) => (Item -> a) -> a -> Book -> Book
deleteFromBook f toDelete oldBook = oldBook {items = filter (\item -> f item /= toDelete) (items oldBook)}

-- delid 123
deleteById :: [String] -> IO ()
deleteById [filepath, idStr] = do
  handle <- openFile filepath ReadMode
  contents <- hGetContents handle
  
  let book = (decodeJSON contents :: Book)

  let originalItems = items book
  -- TODO fail for non-int input
  let updatedBook = deleteFromBook itemId (read idStr :: Int) book
  let newItems = items updatedBook
  
  if originalItems == newItems
    then do
      putStrLn "No match found, nothing done."
    else do
      (tempName, tempHandle) <- openTempFile "." "temp"
      hPutStr tempHandle $ (encodeJSON updatedBook)
      hClose tempHandle
      
      removeFile filepath
      renameFile tempName filepath
      
      let valueStr = value $ head $ originalItems \\ newItems
      putStrLn $ "Value \"" ++ valueStr ++ "\" deleted."
  hClose handle
deleteById _ = error "Syntax: <path to file> delid itemId"

-- tag value +tag2-tag1
changeTags :: [String] -> IO ()
changeTags [filename, value, tagStr] = do
  let (addTags, delTags) = parseChangeTags tagStr
  putStrLn $ show addTags
  putStrLn $ show delTags
  putStrLn "Not yet implemented."

parseChangeTags :: String -> ([Tag], [Tag])
--TODO
parseChangeTags s = ([],[])
