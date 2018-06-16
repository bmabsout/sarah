{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Convenience
import FeaturesHandler
import Exported


import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Control.Monad (forever, join)
import Data.Function ((&))
import Data.Text (Text,unpack)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Prelude as P
import System.Console.ANSI(clearScreen)
import Codec.Xlsx
import Foundation
import Foundation.String
import Foundation.Collection (zip)
import Foundation.IO (hPut, withFile, IOMode(WriteMode),putStrLn)
import System.FilePath (FilePath, takeExtension, takeFileName, replaceExtension)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Basement.From
import Control.Exception


main =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      "."          -- directory to watch
      (\case
            Added p _ -> takeExtension p == ".xlsx" && (takeFileName p & isPrefixOf "~$" & not)
            otherwise -> False
      ) -- predicate
      (eventPath &. loadXlsx)        -- action

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
  tags     <- readSet "tags.txt"
  features <- readSet "features.txt"
  let csvName = replaceExtension fp "csv"
  input
    &> (\(worksheetTitle, worksheetContent) ->
            case getXlsxValues $ worksheetValues worksheetContent of
              Nothing -> Left "Skipping invalid worksheet.."
              Just ws -> handleParseErrors (Tags tags, Features features) ws worksheetTitle
       )
    & P.sequence & second P.concat
    & either putStrLn (writeCsv csvName)



handleParseErrors :: (Tags,Features) -> [[Maybe Cell]] -> Text -> Either String [[String]]
handleParseErrors tf cells title = eatErrors

  where
    indexedAndCleaned :: [(Int,Either Error Row)]
    indexedAndCleaned =
      Lib.toRows fileTitle tf output
      & zip [1..]
      & filter (\(i,r) -> not $ notARow r)
    notARow (Left NotARow) = True
    notARow _ = False

    fileTitle = unpack title & fromList
    output = cells & cellToCellValue & parseCellValue

    eatErrors :: Either String [[String]]
    eatErrors =
      indexedAndCleaned
      &> (\(i, eit) -> (eit >>= duplicateHandler i) & first (toStringParseError i))
      & P.sequence
      &> const (uniqueRows & M.elems &> (snd &. export &.> snd))

    duplicateHandler :: Int -> Row -> Either Error Row
    duplicateHandler currI row =
      case uniqueRows M.!? extractKey row of
        Just (i,otherRow) ->
          let
            sameRows = otherRow {categories = S.empty} == row {categories = S.empty}
            sameCategory = categories row == categories otherRow
            sameIndex = i == currI
            duplicateError extra = Left $ Duplicate extra i (otherRow & categories & S.findMin)
            handleDuplicateErrors
                 | sameRows && sameCategory && not sameIndex =
                     duplicateError "duplicate listing in same category: "
                 | sameRows = Right row
                 | otherwise = duplicateError "Incorrect duplicate of line: "
          in
            handleDuplicateErrors
        Nothing ->
          Right row

    toStringParseError :: Int -> Error -> String
    toStringParseError line parseErr = "Category: " <> fileTitle <> "\n"
                                       <> show line <> ": " <> final parseErr
        where final (ParseError err) = "Error: " <> err
              final (NotARow) = "Error: something's wrong with the row"
              final (Duplicate extra i category) = extra <> show i <> " category: " <> category

    extractKey row = (row & name, row & location)
    uniqueRows :: M.Map (String, Location) (Int, Row)
    uniqueRows =
      indexedAndCleaned
      &> (\(i,eit) -> eit & second (\row -> (i,row)))
      & rights
      &> (\(i,row) -> (extractKey row ,(i,row)))
      & M.fromListWith (\(i,a) (_,b) -> (i,a {categories = (categories a `S.union` categories b)}))

writeCsv :: LString -> [[String]] -> IO ()
writeCsv path cells = do
  putStrLn ("writing to " <> fromList path)
  (titles : cells)
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

