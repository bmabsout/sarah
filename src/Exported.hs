{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exported where

import Foundation
import Foundation.Collection
import Foundation.String
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
    , horrible
    ] &> ($ row)

web2Trees :: [Url] -> [RoseTree]
web2Trees urls =
  let
    nameToUrl =
      urls &> (\(Url subnet address rest) -> (address,subnet <> "." <> address <> "." <> rest))
           & M.fromList
    accoum :: M.Map String String -> String -> (M.Map String String,[String])
    accoum currMap name = (M.delete name currMap,  [name, (currMap M.!? name) & fromMaybe ""])
    (rest, tree) =
      ["facebook", "twitter", "youtube", "instagram", "linkedin", "google"]
      & L.mapAccumL accoum nameToUrl
      & second P.concat
  in (tree <> ["website"] <> [M.elems rest & intersperse "\n" & mconcat])
     &> Singleton

eachDay :: [RangeDayAndTime] -> [(Day,[Range Time])]
eachDay l = l &> dayRangeToList
              & mconcat
              &> second (\e -> [e])
              & M.fromListWith (<>)
              & M.toList
    where dayRangeToList (RangeDayAndTime (Left d) timeRange) = [(d,timeRange)]
          dayRangeToList (RangeDayAndTime (Right (Min from, Max to)) timeRange) = zip [from..to] (P.repeat timeRange)

data RoseTree = Tree [RoseTree] | Singleton String | Number Int deriving (Show)

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


tree :: Row -> RoseTree
tree row =
  Tree
    ([ Singleton "tagline_text"                   , Singleton ""
     , Singleton "video"                          , Singleton ""
     , Singleton "mappin"                         , Singleton ""
     , Singleton "gallery"                        , Singleton ""
     , Singleton "list_price"                     , Singleton ""
     , Singleton "list_price_to"                  , Singleton ""
     , Singleton "Plan_id"                        , Singleton ""
     , Singleton "reviews_ids"                    , Singleton ""
     , Singleton "campaign_id"                    , Singleton ""
     , Singleton "listing_reported_by"            , Singleton ""
     , Singleton "listing_reported"               , Singleton ""
     , Singleton "changed_planid"                 , Singleton ""
     , Singleton "listings_ads_purchase_date"     , Singleton ""
     , Singleton "listings_ads_purchase_packages" , Singleton ""
     , Singleton "gAddress"                       , Singleton (row & location & address)
     , Singleton "latitude"                       , Singleton (row & location & lat & \(bd,ad) -> bd <> "." <> ad)
     , Singleton "longitude"                      , Singleton (row & location & lon & \(bd,ad) -> bd <> "." <> ad)
     , Singleton "phone"                          , Singleton (row & phoneNumbers &> phoneToString & intersperse " / " & mconcat)
     , Singleton "email"                          , Singleton (row & emails & intersperse "\n" & mconcat)
     , Singleton "price_status"                   , Singleton (row & priceSymbol)
     , Singleton "claimed_section"                , Singleton "not_claimed"
     , Singleton "faqs"                           , let empty = Tree [Number 1, Singleton ""]
                                                    in Tree [ Singleton "faq" , empty
                                                            , Singleton "faqans", empty
                                                            ]
     , Singleton "business_hours"                 , row & openingHoursAndDays & eachDay &> dayToTree & mconcat & Tree

     ] <> web2Trees (websites row)
     )

horrible :: Row -> String
horrible = tree &. treeToString

-- data Row = Row
--     { subCategories       :: [String]
--     , name                :: String
--     , location            :: Location
--     , description         :: String
--     , openingHoursAndDays :: [RangeDayAndTime]
--     , ageGroup            :: AgeGroup
--     , ageRange            :: String
--     , priceSymbol         :: String
--     , priceRange          :: String
--     , phoneNumbers        :: [PhoneNumber]
--     , emails              :: [String]
--     , websites            :: [Url]
--     , facilities          :: [String]
--     , photos              :: Bool
--     , academicNotes       :: String
--     , additionalNotes     :: [String]
--     } deriving Show

-- "a:30:{s:12:""tagline_text"";s:9:""kids club"";s:8:""gAddress"";s:55:""Asco Center , Sassine Square, Ashrafieh Beirut, Lebanon"";s:8:""latitude"";s:17:""33.88181741714879"";s:9:""longitude"";s:17:""35.51605207812497"";s:6:""mappin"";s:0:"""";s:5:""phone"";s:8:""01330137"";s:5:""email"";s:19:""info@foxandfrog.com"";s:7:""website"";s:25:""http://www.foxandfrog.com"";s:7:""twitter"";s:0:"""";s:8:""facebook"";s:8:""Fox&Frog"";s:8:""linkedin"";s:0:"""";s:11:""google_plus"";s:0:"""";s:7:""youtube"";s:0:"""";s:9:""instagram"";s:10:""Foxandfrog"";s:5:""video"";s:0:"""";s:7:""gallery"";s:0:"""";s:12:""price_status"";s:6:""notsay"";s:10:""list_price"";s:0:"""";s:13:""list_price_to"";s:0:"""";s:7:""Plan_id"";s:1:""0"";s:11:""reviews_ids"";s:3:""176"";s:15:""claimed_section"";s:11:""not_claimed"";s:26:""listings_ads_purchase_date"";s:0:"""";s:30:""listings_ads_purchase_packages"";s:0:"""";s:4:""faqs"";a:2:{s:3:""faq"";a:1:{i:1;s:0:"""";}s:6:""faqans"";a:1:{i:1;s:0:"""";}}s:14:""business_hours"";a:5:{s:6:""Monday"";a:2:{s:4:""open"";s:5:""09:00"";s:5:""close"";s:5:""17:00"";}s:7:""Tuesday"";a:2:{s:4:""open"";s:5:""09:00"";s:5:""close"";s:5:""17:00"";}s:9:""Wednesday"";a:2:{s:4:""open"";s:5:""09:00"";s:5:""close"";s:5:""17:00"";}s:8:""Thursday"";a:2:{s:4:""open"";s:5:""09:00"";s:5:""close"";s:5:""17:00"";}s:6:""Friday"";a:2:{s:4:""open"";s:5:""09:00"";s:5:""close"";s:5:""17:00"";}}s:11:""campaign_id"";s:0:"""";s:14:""changed_planid"";s:0:"""";s:19:""listing_reported_by"";s:0:"""";s:16:""listing_reported"";s:0:"""";}"

-- "a:30:{s:12:""tagline_text"";s:39:""Aub Alumnis Favorite outdoor Playground"";s:8:""gAddress"";s:67:""Lebanon , Beirut , Corniche el Manara , Paris street , AUB Sea Gate"";s:8:""latitude"";s:9:""33.902263"";s:9:""longitude"";s:9:""35.478886"";s:6:""mappin"";s:0:"""";s:5:""phone"";s:13:""+961 1 350000"";s:5:""email"";s:0:"""";s:7:""website"";s:21:""http://www.aub.edu.lb"";s:7:""twitter"";s:0:"""";s:8:""facebook"";s:0:"""";s:8:""linkedin"";s:0:"""";s:11:""google_plus"";s:0:"""";s:7:""youtube"";s:0:"""";s:9:""instagram"";s:0:"""";s:5:""video"";s:0:"""";s:7:""gallery"";s:0:"""";s:12:""price_status"";s:6:""notsay"";s:10:""list_price"";s:13:""Free Entrance"";s:13:""list_price_to"";s:0:"""";s:7:""Plan_id"";s:1:""0"";s:11:""reviews_ids"";s:0:"""";s:15:""claimed_section"";s:11:""not_claimed"";s:26:""listings_ads_purchase_date"";s:0:"""";s:30:""listings_ads_purchase_packages"";s:0:"""";s:4:""faqs"";a:2:{s:3:""faq"";a:1:{i:1;s:0:"""";}s:6:""faqans"";a:1:{i:1;s:0:"""";}}s:14:""business_hours"";a:7:{s:6:""Monday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}s:7:""Tuesday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}s:9:""Wednesday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}s:8:""Thursday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}s:6:""Friday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}s:8:""Saturday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}s:6:""Sunday"";a:2:{s:4:""open"";s:5:""08:00"";s:5:""close"";s:5:""18:00"";}}s:11:""campaign_id"";s:0:"""";s:14:""changed_planid"";s:0:"""";s:19:""listing_reported_by"";s:0:"""";s:16:""listing_reported"";s:0:"""";}"
