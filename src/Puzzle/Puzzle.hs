{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Puzzle.Puzzle where

import Data.Aeson ( ToJSON(..), FromJSON(..) )
import Web.HttpApiData ( ToHttpApiData(..), FromHttpApiData(..) )
import GHC.Generics ( Generic )

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Text ( Text )
import Data.Monoid ( (<>) )

newtype NET = NET { netxt :: NonEmpty Text } deriving (Show, Eq)

instance ToJSON NET where
  toJSON (NET (a :| bs)) = toJSON (a, bs)

instance FromJSON NET where
  parseJSON v = do
    (a, bs) <- parseJSON v
    return . NET $ a :| bs

data PuzzleType
  = PuzzleListAll
  | PuzzleQAPair
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON PuzzleType
instance FromJSON PuzzleType

-- TODO(MP): There has to be a nicer way to do this . . . TH?
instance ToHttpApiData PuzzleType where
  toUrlPiece PuzzleListAll = "list_all"
  toUrlPiece PuzzleQAPair  = "qa_pair"

instance FromHttpApiData PuzzleType where
  parseUrlPiece "list_all" = Right PuzzleListAll
  parseUrlPiece "qa_pair"  = Right PuzzleQAPair
  parseUrlPiece x          = Left $ "Not a puzzle type: '" <> x <> "'"

data PuzzleCategory
  = Countries
  | Capitals
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON PuzzleCategory
instance FromJSON PuzzleCategory

instance ToHttpApiData PuzzleCategory where
  toUrlPiece Countries = "countries"
  toUrlPiece Capitals  = "capitals"

instance FromHttpApiData PuzzleCategory where
  parseUrlPiece "countries" = Right Countries
  parseUrlPiece "capitals"  = Right Capitals
  parseUrlPiece x           = Left $ "Not a data category: '" <> x <> "'"

data Puzzle
  = ListAll [NET]
  | QAPair [(Text, NET)]
  deriving (Eq, Show, Generic)

instance ToJSON Puzzle
instance FromJSON Puzzle
