module CLI where

import Universum
import Flow

import Lib
import qualified Move

-- very simple console-based UI that works with a single Lib
-- I'm writing this to figure out what the GUI would need

printLib' :: Lib -> IO Lib
printLib' l = printLib l >> pure l

repl :: Lib -> IO Lib
repl l = do
    printLib l
    text <- getLine
    case text of
        "quit" -> pure l
        "back" -> repl <| back l
        "remove" -> repl <| remove l
        "removeR" -> repl (removeR (l^.moves) l |> back)
        "save" -> (Lib.toText l |> writeFile "lib") >> repl l -- name of the lib file is hardcoded for now
        "load" -> do -- this is a bit messy
                    l' <- readFile "lib"
                    case Lib.fromText l' of
                        Just newLib -> repl newLib
                        Nothing -> putTextLn "invalid lib file" >> repl l 

        "mirror" -> repl <| mirror l
        "rotate" -> repl <| rotate l

        _ -> case Move.fromText text of
          Nothing -> putTextLn "unknown command" >> repl l
          Just p  -> repl <| addMove p l

startRepl :: IO Lib
startRepl = repl Lib.empty    
