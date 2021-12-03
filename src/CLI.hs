module CLI where

import Universum
import Flow

import Pos (Move, makeMove')
import qualified Pos
import Point (Point)
import Lib
import qualified Point

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
        "mirror" -> repl <| mirror l
        "rotate" -> repl <| rotate l

        _ -> case Point.fromText text of
          Nothing -> putTextLn "unknown command" >> repl l
          Just p  -> repl <| addMove p l

startRepl :: IO Lib
startRepl = repl Lib.empty    
