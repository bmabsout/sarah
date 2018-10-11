{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Lib
import Convenience
import FeaturesHandler
import Exported
import qualified Images


import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Control.Monad (forever, msum, guard)
import Data.Function ((&))
import Data.Text (Text,unpack)
import qualified Data.Map.Lazy as M
import qualified Prelude as P
import qualified Data.Set as S
import qualified Data.Semigroup as Semigroup
import System.Console.ANSI(clearScreen)
import Codec.Xlsx
import qualified Data.List.NonEmpty as N
import Foundation
import Foundation.String
import Foundation.Collection (zip)
import Foundation.IO (hPut, withFile, IOMode(WriteMode),putStrLn)
import System.FilePath (FilePath, takeExtension, takeFileName, replaceExtension)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Basement.From
import Control.Exception
import qualified Data.Foldable as Foldable
import qualified Data.Set.BKTree as BK
import qualified Text.EditDistance as ED
import Linear

catchAny :: IO () -> IO ()
catchAny n =
  Control.Exception.catch @SomeException n $
      \e -> putStrLn (show e)


main =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      "."          -- directory to watch
      (\case
            Added p _ False -> takeExtension p == ".xlsx" && (takeFileName p & isPrefixOf "~$" & not)
            otherwise -> False
      ) -- predicate
      (eventPath &. loadXlsx &. catchAny)        -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 10000000

print :: Show a => a -> IO ()
print = show &. putStrLn

for = flip fmap

loadXlsx :: FilePath -> IO ()
loadXlsx fp = do
  clearScreen
  bs       <- L.readFile fp
  input    <- getWorksheets $ toXlsx bs
  allFilters <- readAllFilters
  imageMap <- Images.readAllImages
  let csvName = replaceExtension fp "csv"
      filtersCombined = allFilters
                        & M.elems
                        & mconcat
      fuzzyFilters :: Filters
      fuzzyFilters = filtersCombined &> toFuzzy
      allAdditionalNotes = asIndex AdditionalNotes filtersCombined & S.fromList & S.delete ""

  input
    &> (\(worksheetTitle, worksheetContent) ->
            case getXlsxValues $ worksheetValues worksheetContent of
              Nothing -> Left "Skipping invalid worksheet.."
              Just ws -> case M.lookup (unpack worksheetTitle & fromString) allFilters of
                            Nothing -> Left $ "Category "
                                       <> (unpack worksheetTitle & fromString)
                                       <> " is missing a corresponding folder in options"
                            Just _ -> handleParseErrors fuzzyFilters ws worksheetTitle
       )
    & P.sequence & second P.concat
    >>= duplicatesHandler allAdditionalNotes
    >>= wrongFiltersHandler (allFilters &>> S.fromList)
    >>= subCategoriesHandler allFilters
    >>= Images.wrongImagesHandler imageMap
    & either putStrLn (\rows -> do
                          almostDuplicateWarnings (rows &> (\(_,a,_) -> a))
                          writeCsv allAdditionalNotes csvName (rows &> export allAdditionalNotes)
                       )

type Cats = M.Map String (N.NonEmpty Int)

instance {-# OVERLAPS #-} BK.Metric [Char] where
  distance = ED.restrictedDamerauLevenshteinDistance ED.defaultEditCosts

almostDuplicateWarnings :: [Row] -> IO ()
almostDuplicateWarnings l =
    names &> (\name -> (name, fuzzied & BK.delete name & BK.closest name))
          &> (\(name,closest) -> do
                (otherName,dist) <- closest
                guard (dist < (fromIntegral $ toInteger $ length otherName) `div` 4)
                return (name, otherName)
             )
          & catMaybes
          & nubby' (\a b -> fst a `compare` snd b)
          & P.mapM_ (\(name1,name2) -> do
                        putStrLn $ "Warning: Close but not equal: " <> show name1
                        putStrLn $ replicate 30 ' ' <> show name2 <> "\n"
                    )

  where names = l &> name & nub' &> (toList)
        fuzzied = names & BK.fromList


invert :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (<>) pairs
    where pairs = [(v, [k]) | (k, vs) <- M.toList m, v <- vs]

subCategoriesHandler :: M.Map String (V3 [String])
                    -> [([String], Row)]
                    -> Either String [([String], Row)]
subCategoriesHandler filterMap rows =
  rows &> second (\row -> row {subCategories = correctedSubCats row & S.fromList})
       & pure
  where
    correctedSubCats :: Row -> [String]
    correctedSubCats row =
      subCategories row
      & S.toList
      &> (\sub -> (catPerSubCat M.! sub)
                  &> (\cat -> cat <> ">" <> sub)
         )
      & mconcat

    catPerSubCat :: M.Map String [String]
    catPerSubCat = filterMap &> asIndex Subcategories & invert

wrongFiltersHandler :: M.Map String (V3 (S.Set String))
                    -> [(Cats, Row)]
                    -> Either String [([String], Row)]
wrongFiltersHandler filterMap = fmap wrongFilterHandler &. P.sequence
  where
    wrongFilterHandler :: (Cats, Row) -> Either String ([String], Row)
    wrongFilterHandler (cats, row) =
      [ elementsOf Subcategories (subCategories row)
      , elementsOf Amenities (facilities row)
      , elementsOf AdditionalNotes (additionalNotes row)
      ] & Foldable.asum & maybe (Right (catNames,row)) Left

      where
        catNames = M.keys cats
        combinedFilters :: V3 (S.Set String)
        combinedFilters =
          catNames &> (filterMap M.!?) & catMaybes & mconcat

        elementsOf :: FilterType -> S.Set String -> Maybe String
        elementsOf t elems =
          notInCategories & S.toList & N.nonEmpty
          &> (\l -> show (N.toList l)
                    <> "\ndoes not belong in the "
                    <> show t <> " of\n"
                    <> (cats &> N.toList & M.toList & show)
                    <> " which are: \n"
                    <> ((S.toList currentFilters & intersperse "\n" & mconcat))
              )
          where notInCategories = elems S.\\ currentFilters
                currentFilters = asIndex t combinedFilters



duplicatesHandler :: S.Set String -> [(String,Int,Row)] -> Either String [(Cats, Row)]
duplicatesHandler allAdditionalNotes indexedRows =
    indexedRows
    &> duplicateHandler
    & msum
    & maybe (Right $ uniqueRows & M.elems &> second (\(r,c,i) -> r)) Left
  where
    extractKey row = (row & name, row & location)

    uniqueRows :: M.Map (String, Location) (Cats, (Row, String,Int))
    uniqueRows =
      indexedRows
      &> (\(cat,i,row) -> (extractKey row ,(M.singleton cat (i N.:| []),(row,cat,i))))
      & (M.fromListWith $
           \(amap,a) (bmap,b) -> (M.unionWith (Semigroup.<>) amap bmap,a)
        )

    duplicateHandler :: (String, Int, Row) -> Maybe String
    duplicateHandler (currCat,currI,row) = do
      --duplicate rows exist
      (uniqueCats,(uniqueRow,uniqueCat,uniqueI)) <- uniqueRows M.!? extractKey row

      let duplicatesSameCat = do
            duplicateI <- uniqueCats M.!? currCat >>= N.toList &. find (/= currI)
            Just $ "duplicate listing in same category: " <> currCat
                   <> " at " <> show currI <> " and " <> show duplicateI
                   <> "\nnamed: " <> name uniqueRow

          notSameRow =
            let rowMap = rowContent allAdditionalNotes ([],row,mempty)
                uniqueRowMap = rowContent allAdditionalNotes ([],uniqueRow,mempty)
            in
              find (\title -> rowMap M.! title /= uniqueRowMap M.! title) (titles allAdditionalNotes)
              &> \title ->
                   "Wrong duplicate listing: "
                   <> show (currCat, currI)
                   <> ", " <> show (uniqueCat, uniqueI)
                   <> " at: " <> title
                   <> "\nnamed: " <> (name uniqueRow)

      duplicatesSameCat <|> notSameRow



handleParseErrors :: Filters -> [[Maybe Cell]] -> Text -> Either String [(String,Int,Row)]
handleParseErrors options cells title = eatParseErrors

  where
    indexedAndCleaned :: [(Int,Either Error Row)]
    indexedAndCleaned =
      Lib.toRows options output
      & zip [1..]
      & filter (\(i,r) -> not $ notARow r)
    notARow (Left NotARow) = True
    notARow _ = False

    fileTitle = unpack title & fromList
    output = cells & cellToCellValue & parseCellValue

    eatParseErrors :: Either String [(String,Int,Row)]
    eatParseErrors =
      indexedAndCleaned
      &> (\(i, eit) -> eit & bimap (toStringParseError i) (fileTitle,i,))
      & P.sequence


    toStringParseError :: Int -> Error -> String
    toStringParseError line parseErr = "Category: " <> fileTitle <> "\n"
                                       <> show line <> ": " <> final parseErr
        where final (ParseError err) = "Error: " <> err
              final (NotARow) = "Error: something's wrong with the row"


writeCsv :: S.Set String -> LString -> [[String]] -> IO ()
writeCsv allAdditionalNotes path cells = do
  putStrLn ("writing to " <> fromList path)
  (titles allAdditionalNotes : cells)
    &>> (\s -> "\"" <> replace "\"" "\"\"" s <> "\"")
    &> (intersperse "," &. mconcat)
    & (intersperse "\n" &. mconcat)
    & from
    & (\fileData -> withFile (path & fromString) WriteMode (flip hPut fileData))
  putStrLn "done!"



cellToCellValue :: [[Maybe Cell]] -> [[CellValue]]
cellToCellValue cells = fmap (fmap (fromMaybe (CellText ""))) (cellToValues cells)


parseCellValue :: [[CellValue]] -> [[String]]
parseCellValue cellVal = fmap (drop 1) $ drop 1 $ fmap (fmap (showCells)) cellVal

showCells :: CellValue -> String
showCells (CellText text) = fromString $ unpack text
showCells (CellDouble double) = show (P.round double)
showCells (CellBool bool) = show bool
showCells (CellRich richTextRun) = richTextRun &> _richTextRunText & mconcat & unpack & fromString


getWorksheets :: Xlsx -> IO [(Text, Worksheet)]
getWorksheets xlsx = return $ (_xlSheets xlsx)

worksheetValues :: Worksheet -> CellMap
worksheetValues = _wsCells

numberOfRows :: CellMap -> Maybe Int
numberOfRows = fmap maximum . nonEmpty . fmap fst . M.keys

numberOfColumns :: CellMap -> Maybe Int
numberOfColumns = fmap maximum . nonEmpty . fmap fst . M.keys

getXlsxValues :: CellMap -> Maybe [[Maybe Cell]]
getXlsxValues cm = do
  rows <- numberOfRows cm
  cols <- numberOfColumns cm
  return $ for [0..rows] $ \r ->
              for [0..cols] $ \c ->
                  M.lookup (r, c) cm

cellToValues :: [[Maybe Cell]] -> [[Maybe CellValue]]
cellToValues = fmap (fmap (>>= _cellValue))

