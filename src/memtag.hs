import qualified Control.Exception as C
import System.Environment(getArgs)
import System.IO.Error

-- Own modules
import Actions
import Domain
import FileUtil

---------------------------------------
-- Main executer and friends
---------------------------------------
syntaxMsg = "\nSyntax: <path to file> find tag1[(+|-)tag2...[/tagX[(+|-)tagY...]]]"
         ++ "\n                       findv <part of value>"
         ++ "\n                       add value tag1[:tag2...]"
         ++ "\n                       delv value"
         ++ "\n                       del id[,id2...]"
         ++ "\n                       tag id[,id2...] (+|-)tag1[(+|-)tag2...]"
         ++ "\n                       tagsets"
         ++ "\n                       list t|i"
         ++ "\n"

tooFewArgsMsg = "\nToo few arguments." ++ syntaxMsg

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", addValue)
            , ("del", deleteById)
            , ("delv", deleteByValue)
            , ("find", findByTags)
            , ("findv", findByValue)
            , ("tag", changeTags)
            , ("tagsets", listTagSets)
            , ("list", list)
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
