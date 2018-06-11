{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exported where

import Foundation
import Foundation.Collection
import qualified Prelude as P
import Lib
import Data.Semigroup hiding ((<>))
import Convenience
import qualified Data.List as L
import qualified Data.Map.Strict as M

export :: String -> Row -> [String]
export category row =
    [ name
    , description
    , const category
    , facilities &. intersperse "|" &. mconcat
    , \r -> (subCategories r <> additionalNotes r) & intersperse "|" & mconcat
    , const ""
    , ageGroup &. (maybe "" (\(Min from,Max to) -> show from <> "-" <> show to))
    , horrible
    ] &> ($ row)

web2Trees :: [Url] -> [RoseTree]
web2Trees urls =
  let
    nameToUrl =
      urls &> (\(Url subnet address rest) -> (address,subnet <> "." <> address <> "." <> rest))
           & M.fromList
    accoum :: M.Map String String -> String -> (M.Map String String,[String])
    accoum currMap name = (M.delete name currMap,  [iff name (== "google") "google_plus", (currMap M.!? name) & fromMaybe ""])
    (rest, tree) =
      ["twitter","facebook","linkedin","google","youtube","instagram"]
      & L.mapAccumL accoum nameToUrl
      & second P.concat
  in (["website"] <> [M.elems rest & intersperse "\n" & mconcat] <> tree)
     &> Singleton

eachDay :: [RangeDayAndTime] -> [(Day,[Range Time])]
eachDay l = l &> dayRangeToList
              & mconcat
              &> second (\e -> [e])
              & M.fromListWith (<>)
              & M.toList
    where dayRangeToList (RangeDayAndTime (Left d) timeRange) = [(d,timeRange)]
          dayRangeToList (RangeDayAndTime (Right (Min from, Max to)) timeRange) = zip [from..to] (P.repeat timeRange)

data RoseTree = Tree [RoseTree] | Singleton String | Number Int deriving (Show,Eq)

dayToTree :: (Day, [Range Time]) -> [RoseTree]
dayToTree (day, list) =
  [Singleton (show day), Tree [ Singleton "open", Singleton (timesToString opens)
                              , Singleton "close", Singleton (timesToString closeds)
                              ]
  ]
  where (opens,closeds) = unzip list & bimap (fmap getMin) (fmap getMax)
        timesToString l = l &> timeToString & intersperse "\n" & mconcat

treeToString :: RoseTree -> String
treeToString t =
  cons (treeChar t) (":" <> show (childrenSize t) <> ":"
                     <> go t)
  where
    childrenSize :: RoseTree -> Int
    childrenSize (Number i) = i
    childrenSize (Singleton s) = length s & fromCount
    childrenSize (Tree l) = (length l & fromCount) `div` 2

    treeChar (Number _) = 'i'
    treeChar (Singleton _) = 's'
    treeChar (Tree _) = 'a'

    go :: RoseTree -> String
    go (Number s) = ""
    go (Singleton s) = "\"" <> s <> "\";"
    go (Tree l) =
      "{" <> (l &> treeToString & mconcat) <> "}"

toSingleton :: RoseTree -> RoseTree
toSingleton (Tree []) = Singleton ""
toSingleton a = a

tree :: Row -> RoseTree
tree row =
  Tree
    ([ Singleton "tagline_text"                   , Singleton ""
     , Singleton "gAddress"                       , Singleton (row & location & address)
     , Singleton "latitude"                       , Singleton (row & location & lat)
     , Singleton "longitude"                      , Singleton (row & location & lon)
     , Singleton "mappin"                         , Singleton ""
     , Singleton "phone"                          , Singleton (row & phoneNumbers &> phoneToString & intersperse " / " & mconcat)
     , Singleton "email"                          , Singleton (row & emails & intersperse " / " & mconcat)
     ] <> web2Trees (websites row) <>
     [ Singleton "video"                          , Singleton ""
     , Singleton "gallery"                        , Singleton ""
     , Singleton "price_status"                   , Singleton (row & priceSymbol)
     , Singleton "list_price"                     , Singleton ""
     , Singleton "list_price_to"                  , Singleton ""
     , Singleton "Plan_id"                        , Singleton "0"
     , Singleton "reviews_ids"                    , Singleton ""
     , Singleton "claimed_section"                , Singleton "not_claimed"
     , Singleton "listings_ads_purchase_date"     , Singleton ""
     , Singleton "listings_ads_purchase_packages" , Singleton ""
     , Singleton "faqs"                           , let empty = Tree [Number 1, Singleton ""]
                                                    in Tree [ Singleton "faq" , empty
                                                            , Singleton "faqans", empty
                                                            ]
     , Singleton "business_hours"                 , row & openingHoursAndDays & eachDay &> dayToTree & mconcat & Tree & toSingleton
     , Singleton "campaign_id"                    , Singleton ""
     , Singleton "changed_planid"                 , Singleton ""
     , Singleton "listing_reported_by"            , Singleton ""
     , Singleton "listing_reported"               , Singleton ""
     ]
    )


horrible :: Row -> String
horrible = tree &. treeToString
