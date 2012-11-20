import Text.JSON
import System.IO

data Book = Book {
    bookId :: Int
  , items :: [Item]
  } deriving (Show)

data Item = Item {
    value :: String
  , tags :: [Tag]
  } deriving (Show)

type Tag = String

--getJSObject :: Ok (JSObject JSValue) -> JSObject
--getJSObject (Ok x) = x

(!) = flip valFromObj

toBook :: JSObject JSValue -> Result Book
--toBook jsobj = Book {bookId = 1, items = []}
toBook js = do
  bookId <- js ! "id"
--  items <- toItems (js ! "items")
  return Book { bookId = bookId, items = []}

toItems :: JSObject JSValue -> [Item]
toItems x = undefined --todo
toItems x = []


bookToStrOrErrMsg :: Result Book -> String
bookToStrOrErrMsg x = case x of
                        Error msg -> msg
                        Ok y -> show y


main = do
  inFile <- openFile "example.json" ReadMode
  contents <- hGetContents inFile
  
  let json = decode contents :: Result (JSObject JSValue)
  putStrLn $ "decoded: " ++ show json
  putStrLn "---------------------"
  let jsonobj = (\(Ok x) -> x) json
  putStrLn $ show $ jsonobj
  putStrLn "---------------------"
  putStrLn $ show $ bookToStrOrErrMsg (toBook jsonobj)
  
  hClose inFile