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
import Data.Semigroup
import Control.Applicative
import ParserCombinators
import Linear


readFiltersInFile :: FilePath -> IO [Foundation.String]
readFiltersInFile s =
    readFile s
    &> lines
    &>> (Foundation.fromList &. clean)

readFilters :: FilePath -> IO (V3 [Foundation.String])
readFilters fp =
   V3 <$> readFiltersInFile (fp </> "Subcategories.txt")
      <*> readFiltersInFile (fp </> "Amenities.txt")
      <*> readFiltersInFile (fp </> "Additional Details.txt")

readAllFilters :: IO (M.Map Foundation.String (V3 [Foundation.String]))
readAllFilters = do
    categories <- listDirectory "options"

    catsAndFilters <- mapM (\category -> do
        filters <- readFilters ("options" </> category)
        pure (category & Foundation.fromList, filters)
        ) categories
    pure $ M.fromList catsAndFilters

toFuzzy :: [Foundation.String] -> FuzzySet
toFuzzy = fmap (Foundation.toList &. T.pack) &. fromList

data FilterType = Subcategories | Amenities | AdditionalNotes deriving Show

asIndex :: FilterType -> V3 a -> a
asIndex Subcategories (V3 a _ _) = a
asIndex Amenities (V3 _ b _) = b
asIndex AdditionalNotes (V3 _ _ c) = c

instance Semigroup a => Semigroup (V3 a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (V3 a) where
    mconcat = sequenceA &.> mconcat
    mempty = pure mempty
    mappend = liftA2 mappend


type Filters = V3 FuzzySet

