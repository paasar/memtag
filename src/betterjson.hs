{-# LANGUAGE DeriveDataTypeable #-}
{- TODO:
          sort
          special tags (date)
          pretty print
  -}
import Text.JSON
import Text.JSON.Generic
import System.Environment(getArgs)
import System.IO

data Book = Book {
    bookId :: Int
  , items :: [Item]
  } deriving (Eq, Show, Data, Typeable)

data Item = Item {
    value :: String
  , tags :: [Tag]
  } deriving (Eq, Show, Data, Typeable)

type Tag = String

itemsByTags :: [Tag] -> Book -> [Item]
itemsByTags [] _ = []
itemsByTags (t:ts) b = filter (\item -> t `elem` (tags item)) (items b)

main = do
  {-
  Args:
    get: tag1:tag2:tag3
    add: add "value" tag1:tag2
  -}

  inFile <- openFile "example.json" ReadMode
  contents <- hGetContents inFile
  
  putStrLn "---------------------"
  let book = (decodeJSON contents :: Book)
  putStrLn $ show $ book
  
  putStrLn $ last $ tags $ head $ items book
  
  putStrLn $ show $ map value $ itemsByTags [("ruokalista" :: Tag)] book
  
  hClose inFile