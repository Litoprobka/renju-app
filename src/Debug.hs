module Debug where

import DefaultImports

import qualified MoveSeq
import qualified Move
import qualified Lib

-- chances are there is a better way to make debug-only functions, but this works so far

fromJust (Just x) = x
fromJust Nothing = error "Attempted to apply fromJust to Nothing"

pp x y = fromJust <| Move.fromInt x y

pt = fromJust <. Move.fromText

gg = fromJust <. MoveSeq.fromGetpos

mm = MoveSeq.makeMove'

mmp x y = mm <| pp x y

mmt = mm <. pt

tt :: [Text] -> MoveSeq.MoveSeq
tt = foldl' (flip mmt) MoveSeq.empty 

printl = Lib.printLib

ladd :: Text -> Lib.Lib -> Lib.Lib
ladd = Lib.addMove <. pt