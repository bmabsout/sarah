{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module FeaturesHandler(module FeaturesHandler) where

import Data.FuzzySet
import Convenience
import qualified Foundation as Foundation
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import ParserCombinators


readFiltersInFile :: FilePath -> IO [Foundation.String]
readFiltersInFile s =
    readFile s
    &> lines
    &>> (Foundation.fromList &. clean)

readFilters :: FilePath -> IO (FilterType -> [Foundation.String])
readFilters fp = do
   sub  <- readFiltersInFile (fp </> "Subcategories.txt")
   ame  <- readFiltersInFile (fp </> "Amenities.txt")
   adi  <- readFiltersInFile (fp </> "Additional Details.txt")
   pure $ \case
        SubCategories   -> sub
        Amenities       -> ame
        AdditionalNotes -> adi

readAllFilters :: IO (M.Map Foundation.String (FilterType -> [Foundation.String]))
readAllFilters = do
    categories <- listDirectory "options"

    catsAndFilters <- mapM (\category -> do
        filters <- readFilters ("options" </> category)
        pure (category & Foundation.fromList, filters)
        ) categories
    pure $ M.fromList catsAndFilters


toFuzzy :: [Foundation.String] -> FuzzySet
toFuzzy = fmap (Foundation.toList &. T.pack) &. fromList

data FilterType = SubCategories | Amenities | AdditionalNotes deriving Show

type Filters = FilterType -> FuzzySet

combineFilters :: Monoid a => [FilterType -> a] -> FilterType -> a
combineFilters = sequence &.> mconcat
