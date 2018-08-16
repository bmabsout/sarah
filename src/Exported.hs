{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exported(export, titles, rowContent) where

import Foundation
import Foundation.Collection
import qualified Prelude as P
import Lib
import Data.Semigroup hiding ((<>))
import Convenience
import qualified Data.Map as M
import qualified Data.Set as S

infixl 1 =:
(=:) = (,)

titles :: [String]
titles = rowContent ([],emptyRow) & M.keys

export = rowContent &. M.elems

rowContent :: ([String], Row) -> M.Map String String
rowContent (categories, row) =
    ([ "title"          =: name
     , "content"        =: description
     , "categories"     =: const categories &. intersperse "|" &. mconcat
     , "subCategories"  =: subCategories &. intersperseAndCollide "|"
     , "facilities"     =: facilities &. intersperseAndCollide "|"
     , "additionalNotes"=: additionalNotes &. intersperseAndCollide "|"
     , "ages"           =: ageGroup &. (maybe "" (\(Min from, Max to) -> show from <> "-" <> show to))
     , "address"        =: location &. address
     , "latitude"       =: location &. lat
     , "longitude"      =: location &. lon
     , "phone"          =: phoneNumbers &. (S.map phoneToString) &. intersperseAndCollide " / "
     , "email"          =: emails &. intersperseAndCollide " / "
     , "price_status"   =: priceSymbol
     , "business_hours" =: openingHoursAndDays &. S.toList &. eachDay &.> dayToString &. intersperse ". " &. mconcat
     ] &> (second ($ row))
    ) <> web2List (websites row & S.toList)
    & M.fromList
  where intersperseAndCollide seperator = S.toList &. intersperse seperator &. mconcat

web2List :: [Url] -> [(String,String)]
web2List urls = nameToUrl & M.toAscList &> second (filter (not.null) &. intersperse "\n" &. mconcat)
  where
    websiteNames =
      ["twitter","facebook","linkedin","google","youtube","instagram", "website"]
    websiteSet = S.fromList websiteNames
    nameToUrl :: M.Map String [String]
    nameToUrl =
      urls &> (\(Url subnet address rest) -> ( if address `S.member` websiteSet
                                               then address
                                               else "website"
                                             , [subnet <> "." <> address <> "." <> rest]))
           & (<> (websiteNames &> (\name -> (name, [""]))))
           & M.fromListWith (<>)

eachDay :: [RangeDayAndTime] -> [(Day,Range Time)]
eachDay l = l &> dayRangeToList
              & mconcat
    where dayRangeToList (RangeDayAndTime (Left d) timeRange) = [(d,timeRange)]
          dayRangeToList (RangeDayAndTime (Right (Min from, Max to)) timeRange) = zip [from..to] (P.repeat timeRange)

dayToString :: (Day, Range Time) -> String
dayToString (day, (Min from, Max to)) = show day <> ": " <> timeToString from <> " to " <> timeToString to

