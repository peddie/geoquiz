{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Puzzle.Generate where

import           Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Database.Models ( Country(..) )
import           Puzzle.Puzzle

convText :: NonEmpty String -> NET
convText = NET . fmap T.pack

generate :: PuzzleType  -- Puzzle variety
         -> PuzzleCategory  -- What subset of data the answers are about
         -> Int         -- Random seed
         -> [Country]   -- Country database
         -> Puzzle      -- Resulting puzzle
generate PuzzleListAll cat _ = ListAll . map (convText . res cat)
  where
    res Countries = countryName
    res Capitals  = countryCapital
generate PuzzleQAPair cat _ = QAPair . map form
  where
    res Countries = (countryCapital, countryName)
    res Capitals = (countryName, countryCapital)
    (q, a) = res cat
    form c = (T.pack . NE.head . q $ c, convText . a $ c)
