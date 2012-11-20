{-# LANGUAGE DeriveDataTypeable #-}
{- TODO:
    pretty print
      output options:
        sort
        show tags
    special tags (date)
  -}
import System.Environment(getArgs)
import System.IO
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

stringToTags :: String -> [Tag]
stringToTags tagStr = splitRegex (mkRegex ":") tagStr


findByTags :: String -> [Tag] -> IO ()
findByTags filepath tags = do
  inFile <- openFile filepath ReadMode
  contents <- hGetContents inFile
  let book = (decodeJSON contents :: Book)
  putStrLn $ show $ map value $ itemsByTags tags book
  
  hClose inFile
  
itemsByTags :: [Tag] -> Book -> [Item]
itemsByTags [] _ = []
itemsByTags (t:ts) b =
  filter (\item -> t `elem` (tags item)) (items b) ++ (itemsByTags ts b)

addValue :: String -> String -> [Tag] -> IO ()
addValue filepath value tags = do
  putStrLn "Not yet implemented"

main = do
  {-
  Args:
    get: tag1:tag2:tag3
    add: add "value" tag1:tag2
  -}
  args <- getArgs
  case args of
    [filepath, tagStr] -> findByTags filepath (stringToTags tagStr)
    [filepath, "add", value, tagStr] -> addValue filepath value (stringToTags tagStr)
    _ -> putStrLn "Syntax: <path to file> [add value] tag1[:tag2...]"
  
  {-
  putStrLn "---------------------"
  let book = (decodeJSON contents :: Book)
  putStrLn $ show $ book
  putStrLn $ last $ tags $ head $ items book
  putStrLn $ show $ map value $ itemsByTags [("ruokalista" :: Tag)] book
  -}