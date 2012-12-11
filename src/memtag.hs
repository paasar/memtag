{-# LANGUAGE DeriveDataTypeable #-}
{- TODO:
    find by tags by part of tag
    delete Item(s) by tag(s) (delid $ find ids)
    modify value
    pretty print JSON (if python -mjson.tool is not enough)
    output options:
       sort
       hide tags
    special tags (date)
    tag wildcards
    Can't use plus or minus characters in tags
  DONE:
    find Item by tags
    add Item
    pretty print results
    date to Items (done as a automatic tag)
    delete Item by id
    add tag,remove tag (tag value +tag2-tag1)
    list tags
    list all Items
    ~check argument amount
    find Item with tag-modifiers (+tag1-tag2/tag3)
    refactor file handling into one place
    modify tags selector is id
    delid ja modify tags to support multiple ids
    find Item by part of value
    show all used tag combinations
-}
import qualified Control.Exception as C
import Data.List(
                  find
                , intercalate
                , intersect
                , isInfixOf
                , nub
                , sort
                , (\\)
                )
import Data.Time
import System.Environment(getArgs)
import System.Directory
import System.IO
import System.IO.Error
import Text.JSON
import Text.JSON.Generic
import Text.Regex(splitRegex, mkRegex, subRegex)
import Text.Regex.Posix

---------------------------------------
-- Data entities
---------------------------------------
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
-- Main executer and friends
---------------------------------------
syntaxMsg = "\nSyntax: <path to file> find tag1[(+|-)tag2...[/tagX[(+|-)tagY...]]]"
         ++ "\n                       findv <part of value>"
         ++ "\n                       add value tag1[:tag2...]"
         ++ "\n                       delv value"
         ++ "\n                       del id[,id2...]"
         ++ "\n                       tag id[,id2...] (+|-)tag1[(+|-)tag2...]"
         ++ "\n                       list \"t\"/\"i\""
         ++ "\n"

tooFewArgsMsg = "\nToo few arguments." ++ syntaxMsg

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", addValue)
            , ("del", deleteById)
            , ("delv", deleteByValue)
            , ("find", findByTags)
            , ("findv", findByValue)
            , ("tag", changeTags)
            , ("list", list)
            , ("tagsets", listTagSets)
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

----------------------------------
-- Actions
----------------------------------
-- list t/v
list :: [String] -> IO ()
list [filepath, desiredType] = do
  (book, inFile) <- readBook filepath

  if desiredType == "t"
    then do
      putStrLn $ intercalate "\n" $ getTags $ items book
    else do
      putStrLn $ printResult $ items book
   
  hClose inFile

getTags :: [Item] -> [Tag]
getTags [] = []
getTags (item:rest) = sort $ nub $ tags item ++ getTags rest

----------------------------------
-- find RAAH or find tag1+tag2-tag3/tag4/tag5-tag6
findByTags :: [String] -> IO ()
findByTags [filepath, tagStr] = do
  (book, inFile) <- readBook filepath

  let tagSets = split tagStr "/"
  let plusMinusTagSets = map toPlusAndMinusTags $ map stringToPlusMinusParts tagSets
  let foundItems = findByTagSets plusMinusTagSets book
  
  if length foundItems > 0
    then do
      putStrLn $ printResult $ foundItems
    else do
      putStrLn "No matches."
   
  hClose inFile
findByTags _ = error "Syntax: <path to file> find tag1[:tag2...]"

printResult :: [Item] -> String
printResult [] = ""
printResult (item:rest) = show (itemId item) ++ ": " ++ value item
                          ++ " [" ++ (intercalate ", " (tags item)) ++ "]\n"
                          ++ printResult rest

split :: String -> String -> [String]
split tagStr regex = splitRegex (mkRegex regex) tagStr

findByTagSets :: [([Tag], [Tag])] -> Book -> [Item]
findByTagSets sets book = recurseFindItems [] sets (items book)

recurseFindItems :: [Item] -> [([Tag], [Tag])] -> [Item] -> [Item]
recurseFindItems result [] _ = result
recurseFindItems result (set:rest) items' =
  recurseFindItems (
    (excludeItemsByTags (snd set) $ includeItemsByTags (fst set) items')
    ++ result) rest items'

includeItemsByTags :: [Tag] -> [Item] -> [Item]
includeItemsByTags [] _ = []
includeItemsByTags tgs items =
  filter (\item -> length (tgs `intersect` tags item) == length tgs) items

excludeItemsByTags :: [Tag] -> [Item] -> [Item]
excludeItemsByTags [] items = items
excludeItemsByTags tgs items =
  filter (\item -> length (tgs `intersect` tags item) == 0) items

----------------------------------
-- findv "val1"
findByValue :: [String] -> IO ()
findByValue [filepath, searchValue] = do
  (book, inFile) <- readBook filepath
  
  let foundItems =
        filter (\item -> searchValue `isInfixOf` (value item)) $ items book
  
  if length foundItems > 0
    then do
      putStrLn $ printResult $ foundItems
    else do
      putStrLn "No matches."
  hClose inFile
findByValue _ = error "Syntax: <path to file> findv <part of value>"

----------------------------------
-- add "val1" tag1:tag2
addValue :: [String] -> IO ()
addValue [filepath, value, tagStr] = do
  let tags = split tagStr ":" :: [Tag]
  
  (book, inFile) <- readBook filepath
  
  dateTag <- createDateTag
  let nextId = nextItemId $ items book
  let updatedBook = addValueToBook value tags dateTag nextId book
  
  writeBook filepath inFile updatedBook
  
  putStrLn $ "Item \"" ++ value ++ "\" (id: " ++ show nextId ++ ") added."
addValue _ = error "Syntax: <path to file> add value tag1[:tag2...]"

createDateTag :: IO Tag
createDateTag = do
  c <- getCurrentTime
  -- replace minuses with dots because minus is not allowed in tag
  return ("Date " ++ subRegex (mkRegex "-") (show $ utctDay c) "." :: Tag)

addValueToBook :: String -> [Tag] -> Tag -> Int -> Book -> Book
addValueToBook value tags dateTag nextId oldBook =
    oldBook {items = (createItem value tags) : oldItems}
  where createItem value tags = Item { itemId = nextId,
                                       value = value,
                                       tags = dateTag : tags }
        oldItems = (items oldBook)

nextItemId :: [Item] -> Int
nextItemId oldItems = (itemId (foldl1 maxId oldItems)) + 1
  where maxId x y = case compare (itemId x) (itemId y) of
                    GT -> x
                    _  -> y

----------------------------------
-- del "valx"
deleteByValue :: [String] -> IO ()
deleteByValue [filepath, valueStr] = do
  (book, inFile) <- readBook filepath
  
  let originalItems = items book
  let updatedBook = deleteFromBook value valueStr book
  let newItems = items updatedBook
  
  if originalItems == newItems
    then do
      hClose inFile
      putStrLn "No match found, nothing done."
    else do
      writeBook filepath inFile updatedBook
      putStrLn $ "Item \"" ++ valueStr ++ "\" deleted."
deleteByValue _ = error "Syntax: <path to file> del value"

-- | Delete Item by Item attribute.
-- First parameter is the function to extract attribute value.
-- Second parameter is the target value which should match.
deleteFromBook :: (Eq a) => (Item -> a) -> a -> Book -> Book
deleteFromBook f toDelete oldBook = oldBook { items = filter (\item -> f item /= toDelete) (items oldBook) }

----------------------------------
-- delid ids (123 or delid 1,4,15)
deleteById :: [String] -> IO ()
deleteById [filepath, idStr] = do
  (book, inFile) <- readBook filepath

  let originalItems = items book
  -- TODO: fail for non-int input
  let updatedBook = deleteMultipleFromBook itemId (toIntArray idStr) book
  let newItems = items updatedBook
  
  if originalItems == newItems
    then do
      hClose inFile
      putStrLn "No match found, nothing done."
    else do
      writeBook filepath inFile updatedBook
      putStrLn $ "Item(s) " ++ idStr ++ " deleted."
deleteById _ = error "Syntax: <path to file> delid itemId"

toIntArray :: String -> [Int]
toIntArray "" = []
toIntArray idStr = map read $ split idStr ","

deleteMultipleFromBook :: (Eq a) => (Item -> a) -> [a] -> Book -> Book
deleteMultipleFromBook f values oldBook = recurseDeleteFromBook f values oldBook
  where recurseDeleteFromBook f' [] book         = book
        recurseDeleteFromBook f' (val:rest) book =
          recurseDeleteFromBook f' rest (deleteFromBook f' val book)

----------------------------------
-- tag ids +tag2-tag1...
changeTags :: [String] -> IO ()
changeTags [filepath, idStr, tagStr] = do
  let (addTags, delTags) = parseChangeTags tagStr
  
  (book, inFile) <- readBook filepath

  let originalItems = items book
  -- TODO: fail for non-int input
  let updatedBook = modifyMultipleItemTags (toIntArray idStr) addTags delTags book
  let newItems = items updatedBook
  
  if originalItems == newItems
    then do
      hClose inFile
      putStrLn "No match found or tags didn't change, nothing done."
    else do
      writeBook filepath inFile updatedBook
      
      putStrLn $ "Item(s) " ++ idStr ++ " modified:"
      putStrLn $ "  +" ++ show addTags
      putStrLn $ "  -" ++ show delTags
changeTags _ = error "Syntax: <path to file> tag value (+|-)tag1[(+|-)tag2...]"

parseChangeTags :: String -> ([Tag], [Tag])
parseChangeTags str = toPlusAndMinusTags $ stringToPlusMinusParts str

stringToPlusMinusParts :: String -> [String]
-- Take tags from beginning of string or that start with + or -
-- This regex produces duplicates (why?) which are removed with nub
stringToPlusMinusParts tagStr = nub $ concat (tagStr =~ "(^[^+-]+|[+-][^+-]+)" :: [[String]])

toPlusAndMinusTags :: [String] -> ([Tag], [Tag])
toPlusAndMinusTags plusMinusParts = recursePlusMinusTags plusMinusParts [] []
  where recursePlusMinusTags [] plusTags minusTags = (plusTags, minusTags) 
        recursePlusMinusTags (part:rest) plusTags minusTags =
          case (head part) of
            '+' -> recursePlusMinusTags rest (actualTag : plusTags) minusTags
            '-' -> recursePlusMinusTags rest plusTags (actualTag : minusTags)
            -- Elements that does not start with + or - are put in plus array in full
            c   -> recursePlusMinusTags rest (part : plusTags) minusTags
          where actualTag = tail part :: Tag

modifyMultipleItemTags :: [Int] -> [Tag] -> [Tag] -> Book -> Book  
modifyMultipleItemTags ids plusTags minusTags oldBook =
    recurseModifyMultipleItemTags ids plusTags minusTags oldBook
  where recurseModifyMultipleItemTags [] _ _ book = book
        recurseModifyMultipleItemTags (id:rest) plusTags minusTags book =
          recurseModifyMultipleItemTags rest plusTags minusTags (modifyItemTags id plusTags minusTags book)

modifyItemTags :: Int -> [Tag] -> [Tag] -> Book -> Book
modifyItemTags targetId plusTags minusTags oldBook =
    oldBook { items = modifyTagsById (items oldBook) targetId plusTags minusTags }
  where modifyTagsById items targetId' plusTags' minusTags' =
          case (find (\item -> itemId item == targetId') items) of
            Nothing    -> items
            Just item' -> modifyTags item' plusTags' minusTags'
                          : filter (\item'' -> itemId item'' /= targetId') items

modifyTags :: Item -> [Tag] -> [Tag] -> Item
modifyTags orig plusTags minusTags = orig { tags = nub $
  plusTags ++ filter (\tag -> tag `notElem` minusTags) (tags orig) }

----------------------------------
-- tagsets
listTagSets :: [String] -> IO ()
listTagSets [filepath] = do
  (book, inFile) <- readBook filepath
  
  let tagSets = findTagSets book
  
  if length tagSets > 0
    then do
      putStrLn $ printTagSets tagSets
    else do
      putStrLn "Could not find any tags."
    
  hClose inFile

findTagSets :: Book -> [[Tag]]
findTagSets book = nub $ recurseTagSets [] (items book)
  where recurseTagSets result [] = result
        recurseTagSets result (item:rest) = sort (tags item) : recurseTagSets result rest

printTagSets :: [[Tag]] -> String
printTagSets [] = ""
printTagSets (tagSet:rest) = "\"" ++ (intercalate "+" tagSet) ++ "\""
                             ++ "\n" ++ printTagSets rest
