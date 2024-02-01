{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module PosID where

import DefaultImports

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)

import Move (Move)
import MoveSeq (LongHash, MoveSeq (..))
import MoveSeq qualified

{- | A MoveSeq-like type that's a bit more compact and only used as key of the Lib hash map

The only reason it hash blackMoves and whiteMoves is to preserve move order when serializing
-}
data PosID = PosID
  { _blackMoves :: Seq Move
  , _whiteMoves :: Seq Move
  , hash :: LongHash
  }
  deriving (Show)

instance Eq PosID where
  (==) = (==) `on` hash

instance Hashable PosID where
  hashWithSalt salt = hashWithSalt salt <. hash

instance ToJSON PosID where
  toJSON = String <. toGetpos

instance ToJSONKey PosID where
  toJSONKey = toJSONKeyText toGetpos
instance FromJSON PosID where
  parseJSON = withText "PosID" parser
   where
    parser (MoveSeq.fromGetpos -> Just pos) = pure <| fromMoveSeq pos
    parser txt = fail <| "cannot parse " <> show txt <> " into PosID"

instance FromJSONKey PosID where
  fromJSONKey = FromJSONKeyTextParser parser
   where
    parser (MoveSeq.fromGetpos -> Just pos) = pure <| fromMoveSeq pos
    parser k = fail <| "cannot parse key " <> show k <> " into PosID"

-- | /O(1)./ Convert a MoveSeq to PosID
fromMoveSeq :: MoveSeq -> PosID
fromMoveSeq moveSeq@MoveSeq{..} = PosID{..}
 where
  hash = MoveSeq.hash moveSeq

-- | /O(n)./ Convert a PosID to MoveSeq
toMoveSeq :: PosID -> MoveSeq
toMoveSeq PosID{..} = hashlessMS |> MoveSeq.moveList |> MoveSeq.fromList
 where
  _hashes = pure 0
  hashlessMS = MoveSeq{..}

-- | /O(n)./ A wrapper around MoveSeq.toGetpos
toGetpos :: PosID -> Text
toGetpos PosID{..} = MoveSeq.toGetpos MoveSeq{..}
 where
  _hashes = pure 0 -- yes, this is a bit hacky
