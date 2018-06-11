{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.Map.Lazy (keys,lookup)
import qualified Prelude as P
import System.Console.ANSI(clearScreen)
import Codec.Xlsx
import Foundation
import qualified Foundation.Format.CSV as CSV
import Foundation.Collection (zip)
import Foundation.IO (hPut, withFile, IOMode(WriteMode),putStrLn)
import System.FilePath (FilePath, takeExtension, takeFileName, replaceExtension)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Basement.Block.Builder
import Basement.From


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
handleParseErrors tf cells title =
    Lib.toRows tf output
      & zip [1..]
      & filter (\(i,r) -> not $ notARow r)
      & eatEither

  where
    notARow (Left NotARow) = True
    notARow _ = False

    fileTitle = unpack title & fromList
    output = cells & cellToCellValue & parseCellValue

    eatEither :: [(Int,Either Error Row)] -> Either String [[String]]
    eatEither l =
      l &> (\(i, eit) -> eit & bimap (toStringParseError i) (export fileTitle))
        & P.sequence

    toStringParseError :: Int -> Error -> String
    toStringParseError line parseErr = "Category: " <> fileTitle <> "\n"
                                       <> show line <> ": " <> final parseErr
        where final (ParseError err) = "Error: " <> err
              final (NotARow) = "Error: something's wrong with the row"



writeCsv :: LString -> [[String]] -> IO ()
writeCsv path cells = do
  putStrLn ("writing to " <> fromList path)
  (titles:cells)
    &>> CSV.toField
    &> fromList
    & fromList
    & CSV.csvBlockBuilder
    & run
    &> from
    &> (\fileData -> withFile (path & fromString) WriteMode (flip hPut fileData))
    & join
  putStrLn "done!"
  where
    titles = ["Title", "Content", "Categories", "Features", "Tags", "Location", "Ages", "lp_listingpro_options"]



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
numberOfRows = fmap maximum . nonEmpty . fmap fst . keys

numberOfColumns :: CellMap -> Maybe Int
numberOfColumns = fmap maximum . nonEmpty . fmap fst . keys

getXlsxValues :: CellMap -> Maybe [[Maybe Cell]]
getXlsxValues cm = do
  rows <- numberOfRows cm
  cols <- numberOfColumns cm
  return $ for [0..rows] $ \r ->
              for [0..cols] $ \c ->
                  lookup (r, c) cm

cellToValues :: [[Maybe Cell]] -> [[Maybe CellValue]]
cellToValues = fmap (fmap (>>= _cellValue))



