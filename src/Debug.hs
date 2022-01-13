module Debug where

import DefaultImports

import qualified MoveSet
import qualified Move
import qualified Lib

-- chances are there is a better way to make debug-only functions, but this works so far

fromJust :: Maybe p -> p
fromJust (Just x) = x
fromJust Nothing = error "Attempted to apply fromJust to Nothing"

pp :: Int -> Int -> Move.Move
pp x y = fromJust <| Move.fromInt x y

pt :: Text -> Move.Move
pt = fromJust <. Move.fromText

gg :: Text -> MoveSet.MoveSet
gg = fromJust <. MoveSet.fromGetpos

mm :: Move.Move -> MoveSet.MoveSet -> MoveSet.MoveSet
mm = MoveSet.makeMove'

mmp :: Int -> Int -> MoveSet.MoveSet -> MoveSet.MoveSet
mmp x y = mm <| pp x y

mmt :: Text -> MoveSet.MoveSet -> MoveSet.MoveSet
mmt = mm <. pt

tt :: [Text] -> MoveSet.MoveSet
tt = foldl' (flip mmt) MoveSet.empty 

printl :: Lib.Lib -> IO ()
printl = Lib.printLib

ladd :: Text -> Lib.Lib -> Lib.Lib
ladd = Lib.addMove <. pt