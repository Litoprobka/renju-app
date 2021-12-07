module Debug where

import Universum
import Flow

import qualified Pos
import qualified Move
import qualified Lib

-- chances are there is a better way to make debug-only functions, but this works so far

fromJust (Just x) = x
fromJust Nothing = error "Attempted to apply fromJust to Nothing"

pp x y = fromJust <| Move.fromInt x y

pt = fromJust <. Move.fromText

gg = fromJust <. Pos.fromGetpos

mm = Pos.makeMove'

mmp x y = mm <| pp x y

mmt = mm <. pt

tt :: [Text] -> Pos.Pos
tt = foldl' (flip mmt) Pos.empty 

printp = Pos.printPos

printl = Lib.printLib

ladd :: Text -> Lib.Lib -> Lib.Lib
ladd = Lib.addMove <. pt