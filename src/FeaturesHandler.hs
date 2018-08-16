{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FeaturesHandler(module FeaturesHandler) where

import Data.FuzzySet
import Convenience
import qualified Foundation.String as FS
import qualified Foundation as Foundation

import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import ParserCombinators


readSet :: FilePath -> IO FuzzySet
readSet s = readFile s &> lines &>> (Foundation.fromList &. clean &. Foundation.toList &. T.pack) &> fromList

readFilters :: FilePath -> IO Filters
readFilters fp =
    (,,)
    <$> (readSet (fp </> "Filters.txt") &> SubCategories)
    <*> (readSet (fp </> "Amenities.txt") &> Amenities)
    <*> (readSet (fp </> "Additional Details.txt") &> AdditionalNotes)

readAllFilters :: IO (M.Map String Filters)
readAllFilters = do
    categories <- listDirectory "options"

    catsAndFilters <- mapM (\category -> do
        filters <- readFilters ("options" </> category)
        pure (category, filters)
        ) categories
    pure $ M.fromList catsAndFilters

newtype SubCategories a = SubCategories a deriving (Show, Eq, Monoid)
newtype Amenities a = Amenities a deriving (Show, Eq, Monoid)
newtype AdditionalNotes a = AdditionalNotes a deriving (Show, Eq, Monoid)

type Filters = (SubCategories FuzzySet, Amenities FuzzySet, AdditionalNotes FuzzySet)

filterMap f (SubCategories s, Amenities am, AdditionalNotes ad) =
    (SubCategories $ f s, Amenities $ f am, AdditionalNotes $ f ad)
