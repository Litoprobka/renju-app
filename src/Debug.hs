module Debug where

import DefaultImports

import qualified MoveSeq
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

gg :: Text -> MoveSeq.MoveSeq
gg = fromJust <. MoveSeq.fromGetpos

mm :: Move.Move -> MoveSeq.MoveSeq -> MoveSeq.MoveSeq
mm = MoveSeq.makeMove'

mmp :: Int -> Int -> MoveSeq.MoveSeq -> MoveSeq.MoveSeq
mmp x y = mm <| pp x y

mmt :: Text -> MoveSeq.MoveSeq -> MoveSeq.MoveSeq
mmt = mm <. pt

tt :: [Text] -> MoveSeq.MoveSeq
tt = foldl' (flip mmt) MoveSeq.empty 

printl :: Lib.Lib -> IO ()
printl = Lib.printLib

ladd :: Text -> Lib.Lib -> Lib.Lib
ladd = Lib.addMove <. pt