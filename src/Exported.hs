{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

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
     , "business_hours" =: openingHoursAndDays &. S.toList &. eachDay &. daysToWeirdString
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

data Trie = A [Trie] | S String

showTrie :: Trie -> String
showTrie (S s) = "s:" <> show (length s & toInteger) <> ":\"" <> s <> "\";"
showTrie (A a) = "a:" <> show (length a & toInteger & (`div` 2)) <> ":{" <> (a &> showTrie & mconcat) <> "}"

daysToWeirdString :: [(Day, Range Time)] -> String
daysToWeirdString =
  fmap (\(day, (Min from, Max to)) ->
          [ S (show day)
          , A [ S "open"
              , S (timeToString from)
              , S "close"
              , S (timeToString to)
              ]
          ]
       )
  &. mconcat &. A &. showTrie


-- a:5:{s:6:"Monday";a:2:{s:4:"open";s:5:"09:00";s:5:"close";s:5:"17:00";}s:7:"Tuesday";a:2:{s:4:"open";s:5:"09:00";s:5:"close";s:5:"17:00";}s:9:"Wednesday";a:2:{s:4:"open";s:5:"09:00";s:5:"close";s:5:"17:00";}s:8:"Thursday";a:2:{s:4:"open";s:5:"09:00";s:5:"close";s:5:"17:00";}s:6:"Friday";a:2:{s:4:"open";s:5:"09:00";s:5:"close";s:5:"17:00";}}
