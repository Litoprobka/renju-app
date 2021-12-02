module Debug where

import Universum hiding ((<.))
import Flow

import qualified Pos
import qualified Point
import qualified Lib

-- chances are there is a better way to do it

fromJust (Just x) = x
fromJust Nothing = error "Attempted to apply fromJust to Nothing"

pp x y = fromJust <| Point.fromInt x y

pt = fromJust <. Point.fromText

gg = fromJust <. Pos.fromGetpos

mm = Pos.makeMove'

mmp x y = mm <| pp x y

mmt = mm <. pt

tt :: [Text] -> Pos.Pos
tt = foldl' (flip mmt) Pos.empty 

printp = Pos.printPos

printl = Lib.printLibAt

ladd = Lib.add <. gg