{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Convenience
import FeaturesHandler


import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text,unpack)
import Data.Map.Lazy (keys,lookup)
import qualified Prelude as Prelude
import System.Console.ANSI(clearScreen)

import Codec.Xlsx
import Foundation
import Foundation.Collection (zip)
import Foundation.IO (hPut, withFile, IOMode(WriteMode),putStrLn)
import Foundation.String (toBytes, Encoding (UTF8))
import Data.Either
import qualified Foundation.VFS.FilePath as FP
import System.FilePath (FilePath, takeExtension)
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)


main =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      "."          -- directory to watch
      (\case
           Added path _ -> takeExtension path == ".xlsx"
           Modified path _ -> takeExtension path == ".xlsx"
           otherwise -> False
      ) -- predicate
      (\case
           Added path _ -> loadXlsx path
           Modified path _ -> loadXlsx path
           otherwise -> mempty
       )        -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

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
  forM_ input $ \(worksheetTitle, worksheetContent) ->
    case getXlsxValues $ worksheetValues worksheetContent of
      Nothing -> putStrLn "Skipping invalid worksheet.."
      Just ws -> printParseErrors (Tags tags, Features features) ws worksheetTitle

toStringParseError (line,x) = show line <> ": " <> final x
    where final (Left x) = show x
          final (Right (ParseError x)) = "Error: " <> x
          final (Right (NotARow)) = "Error: something's wrong with the row"

printParseErrors :: (Tags,Features) -> [[Maybe Cell]] -> Text -> IO ()
printParseErrors tf cells title = do
    let fileTitle = unpack title
    let output = cells & cellToCellValue & parseCellValue :: [[String]]
    putStrLn (fileTitle & fromList)
    -- print (output & nonEmpty &> head :: Maybe [String])
    Lib.toRows tf output
      & zip [1..] & firstError & Prelude.mapM_ (toStringParseError &. putStrLn)
  where
    allPhones :: [(Int,Either Row Error)] -> [(Int, Either [String] Error)]
    allPhones l = l & filter (snd &. isLeft)
                    &> second (first (phoneNumbers &.> phoneToString))

    firstError :: [(Int,Either Row Error)] -> Maybe (Int, Either Row Error)
    firstError =
        find (\(i,r) ->
                  case r of
                    Right (ParseError err) -> True
                    otherwise -> False
             )
writeCsv :: [[Maybe Cell]] -> Text -> IO ()
writeCsv cells title = do
  let fileTitle = unpack title
  let output    = cells & cellToCellValue & parseCellValue & commaSeparator & unlineString
  withFile (fromString (fileTitle <> ".csv") :: FP.FilePath) WriteMode $ \handle -> hPut handle (toBytes UTF8 output)

cellToCellValue :: [[Maybe Cell]] -> [[CellValue]]
cellToCellValue cells = fmap (fmap (fromMaybe (CellText ""))) (cellToValues cells)

-- cellToString :: [[Maybe Cell]] -> [[String]]

parseCellValue :: [[CellValue]] -> [[String]]
parseCellValue cellVal = fmap (drop 1) $ drop 1 $ fmap (fmap (showCells)) cellVal

commaSeparator :: [[String]] -> [String]
commaSeparator = fmap (intercalate ",")

showCells :: CellValue -> String
showCells (CellText text) = fromString $ unpack text
showCells (CellDouble double) = show (Prelude.round double)
showCells (CellBool bool) = show bool
showCells (CellRich richTextRun) = show richTextRun

unlineString :: [String] -> String
unlineString = intercalate "\n"

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

drain = loadXlsx "new.xlsx"


