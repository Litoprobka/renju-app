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
        "removeR" -> repl <| removeR (l^.moves) l
        "mirror" -> repl <| mirror l
        "rotate" -> repl <| rotate l

        _ -> case Move.fromText text of
          Nothing -> putTextLn "unknown command" >> repl l
          Just p  -> repl <| addMove p l

startRepl :: IO Lib
startRepl = repl Lib.empty    
