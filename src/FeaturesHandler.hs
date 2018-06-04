{-# LANGUAGE OverloadedStrings #-}

module FeaturesHandler(module FeaturesHandler) where

import Data.FuzzySet
import Convenience
import qualified Data.Text as T


readSet :: FilePath -> IO FuzzySet
readSet s = readFile s &> lines &>> T.pack &> fromList


newtype Tags = Tags FuzzySet
newtype Features = Features FuzzySet
