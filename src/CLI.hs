module CLI where

import Universum
import Flow

import Pos (Move, makeMove')
import qualified Pos
import Lib (Lib)
import Point (Point)
import qualified Lib
import qualified Point

-- very simple console-based UI that works with a single Lib
-- I'm writing this to figure out what the GUI would need

data LibState = LibState {
      lib :: Lib
    , moves :: [Point]
}

empty = LibState Lib.empty []

back :: LibState -> LibState
back l@(LibState _ []) = l
back l@(LibState _ (m:ms)) = l { moves = ms }

delete :: LibState -> LibState
delete (LibState lib ms) = LibState (lib |> Lib.remove (Pos.fromPointList ms)) ms |> back

transform :: (Point -> Point) -> LibState -> LibState
transform f l = l { moves = moves l <&> f }

mirror :: LibState -> LibState
mirror = transform (\ p -> Point.fromIntPartial (14 - Point.x p) (Point.y p))

rotate :: LibState -> LibState
rotate = transform (\ p -> Point.fromIntPartial (Point.y p) (14-Point.x p))

makeMove :: Point -> LibState -> LibState
makeMove p (LibState lib ms) = LibState (Lib.add (Pos.fromPointList ms |> Pos.makeMove' p) lib) (p : ms) 

printLibState :: LibState -> IO LibState
printLibState l@(LibState lib ms) = Lib.printLibAt (Pos.fromPointList ms) lib >> pure l
 

repl :: LibState -> IO LibState
repl l = do
    printLibState l
    text <- getLine
    case text of
        "quit" -> pure l
        "back" -> repl <| back l 
        "delete" -> repl <| delete l 
        "mirror" -> repl <| mirror l
        "rotate" -> repl <| rotate l

        _ -> case Point.fromText text of
          Nothing -> putTextLn "unknown command" >> repl l
          Just p  -> repl <| makeMove p l

startRepl :: IO LibState
startRepl = repl CLI.empty    
