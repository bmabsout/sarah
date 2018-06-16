{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
-- # LANGUAGE DeriveGeneric #

module Lib
    ( module Lib
    ) where

import Data.Semigroup
import Foundation as F hiding ((<>))
import Foundation.String
import Foundation.Parser as P
import Data.Char (isDigit, isAscii, isAlphaNum)
import Convenience
import ParserCombinators
import Data.Traversable
import Control.Monad
import qualified Data.Text as T
import FeaturesHandler
import qualified Data.FuzzySet as F
import qualified Data.Set as S
import qualified Prelude as Prelude

data Location = Coords (Natural,Natural) (Natural,Natural) String
              | Address String
              deriving (Show, Eq, Ord)

address (Coords _ _ a) = a
address (Address a) = a

showCoord (a,b) = show a <> "." <> show b

lat (Coords l _ _) = showCoord l
lat (Address _) = ""

lon (Coords _ l _) = showCoord l
lon (Address _) = ""


data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
         deriving (Show, Enum, Eq, Ord, Bounded)

data AmPm = Am | Pm deriving (Show, Eq, Ord)
data Time = Time {hour :: Natural, minute :: Natural, ampm :: AmPm} deriving (Show, Eq, Ord)
timeToString :: Time -> String
timeToString (Time hour minute ampm) = pad (show hour) <> ":" <> pad (show minute) <> " " <> lower (show ampm)
    where pad s
            | length s == 0 = "00"
            | length s == 1 = "0" <> s
            | otherwise     = s
data RangeDayAndTime = RangeDayAndTime (Either Day (Range Day)) (Range Time) deriving (Show,Eq,Ord)
data PhoneNumber = PhoneNumber {countryCode :: String, areaCode :: String, number :: String, extension :: Maybe String} deriving (Show,Eq,Ord)
data Url = Url {subnet :: String, urlAddress :: String, rest :: String} deriving (Show,Eq,Ord)

type Range a = (Min a, Max a)
type AgeGroup = Maybe (Range Natural)

phoneToString :: PhoneNumber -> String
phoneToString p = "(+"<>countryCode p<>") "
                  <> (let c = areaCode p in if length c == 1 then "0" <> c else c)
                  <> "-"
                  <> number p
                  <> maybe "" (" ext " <>) (extension p)

data Row = Row
    { categories          :: S.Set String
    , subCategories       :: S.Set String
    , name                :: String
    , location            :: Location
    , description         :: String
    , openingHoursAndDays :: S.Set RangeDayAndTime
    , ageGroup            :: AgeGroup
    , ageRange            :: String
    , priceSymbol         :: String
    , priceRange          :: String
    , phoneNumbers        :: S.Set PhoneNumber
    , emails              :: S.Set String
    , websites            :: S.Set Url
    , facilities          :: S.Set String
    , photos              :: String
    , academicNotes       :: String
    , additionalNotes     :: S.Set String
    } deriving (Show, Eq)

emptyRow = Row mempty mempty "" (Address "") "" mempty Nothing "" "" "" mempty mempty mempty mempty "" "" mempty

data Error = ParseError String | NotARow | Duplicate String Int String deriving (Show,Eq)

toRows :: String -> (Tags,Features) -> [[String]] -> [Either Error Row]
toRows category tf = fmap (toRow category tf)

toRow :: String -> (Tags,Features) -> [String] -> Either Error Row
toRow _ _ [] = Left NotARow
toRow category tf row@(firstCol:_)
    | firstCol == "" || lower firstCol == "sub category" = Left NotARow
    | otherwise =
        let clean s = replace "_x000D_" "\n" s & trim & filter (\c -> isAscii c || isAlphaNum c)
        in row & fixRowLength &> clean & parseRow category tf & first (show &. ParseError)

combineRowsCategories :: [Row] -> [Row]
combineRowsCategories r = []

fixRowLength :: [String] -> [String]
fixRowLength l
    | size > 16 = F.take 16 l
    | size < 16 = l <> (replicate (fromMaybe (CountOf 0)  $ CountOf 16 - size) "" :: [String])
    | otherwise = l
    where size = length l

parseLocation :: String -> Either String Location
parseLocation "" = Right (Address "")
parseLocation s = parseAll (
        Coords <$> parseDecimal <* delimitDecimals <*> parseDecimal <* optional delimitDecimals <*> takeAll
        <|> Address <$> parseNonDecimal) s
    where parseDecimal = (,) <$> (digits <* element '.')
                             <*> digits
          parseNonDecimal = (\a b -> fromList [a] <> b) <$> satisfy_ (not.isDigit) <*> P.takeAll
          delimitDecimals = some' (regularDelimiters <|> space)

range min max = (Min min, Max max)


parseRangeDayAndTime :: Parser String RangeDayAndTime
parseRangeDayAndTime = RangeDayAndTime <$> ((rangeParser dayParser &> Right) <|> (dayParser &> Left))
                                               <* some' space
                                               <* optional (elements "from" <* some' space
                                            )
                                       <*> rangeParser timeParser
  where
    rangeParser :: Parser String a -> Parser String (Range a)
    rangeParser p = range <$> p <* some' space
                                <* (elements "till" <|> elements "til" <|> elements "to")
                                <* some' space
                          <*> p
    dayParser :: Parser String Day
    dayParser =
        (elements     "monday"    &> const Monday)
        <|> (elements "tuesday"   &> const Tuesday)
        <|> (elements "wednesday" &> const Wednesday)
        <|> (elements "thursday"  &> const Thursday)
        <|> (elements "friday"    &> const Friday)
        <|> (elements "saturday"  &> const Saturday)
        <|> (elements "sunday"    &> const Sunday)

    timeParser :: Parser String Time
    timeParser = do
        hour <- digits <* spaces
        minute <- (optional (element ':' *> spaces) *> digits
                   <|> pure 0
                   ) <* spaces
        ampm <- (elements "am" &> const Am) <|> (elements "pm" &> const Pm)
        return $ Time hour minute ampm

parseAgeGroup :: String -> Either String AgeGroup
parseAgeGroup = divideUsual singularGroupParser
                &>> Prelude.foldl (Data.Semigroup.<>) Nothing
    where
        singularGroupParser :: Parser String AgeGroup
        singularGroupParser =
            (\min max -> Just (Min min, Max max))
                <$> digits <* spaces <* element '-' <* spaces
                <*> digits <* spaces
            <|> elements "adult" <* optional (element 's') <* spaces *> pure Nothing


parseWebsites :: String -> Either String (S.Set Url)
parseWebsites = divideAndParse (some' (space <|> newLine)) parseWebsite
    where
        parseWebsite :: Parser String Url
        parseWebsite =
                Url <$> tillDot
                    <*> tillDot
                    <*> takeAll
            <|>
                Url <$> pure "www"
                    <*> tillDot
                    <*> takeAll
            where tillDot = P.takeWhile (/= '.') <* element '.'

parsePhoneNumber :: Parser String PhoneNumber
parsePhoneNumber = do
    countryC <- ((elements "00" <|> element '+') *> numDigits 1 3 <* some' space <|> pure "961" )
    let getLocationCode all locationSize =
            repeat (Exactly all) (spaces *> digit)
            &> fromList
            &> splitAt (CountOf locationSize)
    (locC,numC) <- let normalLocation = getLocationCode 8 2 <|> getLocationCode 7 1
                   in (if countryC == "961"
                       then normalLocation
                       else getLocationCode 9 2 <|> normalLocation
                      )
    ext <- (Just <$> (some' space <* elements "ext" <* some' space *> some digit &> fromList))
           <|> pure Nothing
    return $ PhoneNumber countryC locC numC ext
    where numDigits from to = repeat (Between $ from `And` to) digit &> fromList


parseFromSet :: F.FuzzySet -> String -> Either String (S.Set String)
parseFromSet set s =
    divideUsualStrings s
    &> S.toList
    &>> (\matchWith -> fuzzyDecider (F.get set (stringToText matchWith)) matchWith)
    &> sequenceA
    & join
    &> S.fromList
  where
        fuzzyDecider _ "" = Left "there's an empty word"
        fuzzyDecider [] m = Left "text file is empty"
        fuzzyDecider ((score,deTextMe):_) matchWith
            | score >= 0.7 = Right closestMatch
            | otherwise   = Left $ "couldn't find '"
                                   <> matchWith
                                   <> "' closest I have is "
                                   <> "' " <> closestMatch
                                   <> "' with score: " <> show score
          where closestMatch = deTextMe & T.unpack & fromList

        stringToText :: String -> T.Text
        stringToText = toList &. T.pack

parseEmails = divideAndParse (spacedDelimiter (regularDelimiters <|> string " -" <|> string "- ")) email
    where email = (\a b -> a <> "@" <> b) <$> P.takeWhile (/= '@') <* element '@' <*> P.takeAll

parseRow :: String -> (Tags,Features) -> [String] -> Either (String,String) Row
parseRow category (Tags tags, Features features)
    [ subCategories
    , name
    , location
    , description
    , openingHoursAndDays
    , ageGroup
    , ageRange
    , priceSymbol
    , priceRange
    , phoneNumbers
    , emails
    , websites
    , facilities
    , photos
    , academicNotes
    , additionalNotes
    ]
    =
        (Row (S.singleton category))
        <$> (parseFromSet tags subCategories                              & addCaption "subCategories")
        <*> (asIs name                                                    & addCaption "name")
        <*> (parseLocation location                                       & addCaption "location")
        <*> (asIs description                                             & addCaption "description")
        <*> (divideUsual parseRangeDayAndTime (lower openingHoursAndDays) & addCaption "openingHoursAndDays")
        <*> (parseAgeGroup (lower ageGroup)                               & addCaption "ageGroup")
        <*> (asIs ageRange                                                & addCaption "ageRange")
        <*> (asIs priceSymbol                                             & addCaption "priceSymbol")
        <*> (asIs priceRange                                              & addCaption "priceRange")
        <*> (divideUsual parsePhoneNumber (lower phoneNumbers)            & addCaption ("phoneNumbers: " <> phoneNumbers))
        <*> (parseEmails (lower emails)                                   & addCaption "emails")
        <*> (parseWebsites (lower websites)                               & addCaption "websites")
        <*> (parseFromSet features facilities                             & addCaption "facilities")
        <*> (asIs photos                                                  & addCaption "photos")
        <*> (asIs academicNotes                                           & addCaption "academicNotes")
        <*> (parseFromSet tags additionalNotes                            & addCaption "additionalNotes")
    where addCaption s = first (\e -> (e,s))

example :: [String]
example =
    [ "Hair Salon"
    , "Piixelz"
    , "33.869480, 35.537090  Le Mall Sin El Fil, Habtour Road"
    , "PiiXELZ vision and aim is to create a total new, fun and exclusive experience for both Kids and Parents, an environment filled with beauty and love, and believes in delivering the highest level of quality service to its customers.PiiXELZ will be the leader in its field and be well known in the beauty industry. PiiXELZ sells beautiy products and accessories (spray, perfume, toothbrush, brush, showergel, conditioner and shampoo)"
    , "monday till sunday 10 am till 10 pm"
    , "0-1|1-4|8-12|Adult"
    , "9months to 10 years"
    , "$"
    , " 22,000 for boys25,000 for girls"
    , "1685185\n +96170975965"
    , "info@piixelz.com"
    , "www.piixelz.com/\nwww.facebook.com/Piixelz\nwww.instagram.com/piixelz_kidz"
    , "na"
    , "yes"
    , "na"
    , "parking, valet parking, accepts credit card, requires booking"
    ] &> trim

example2 :: [String]
example2 =
    [ "Daycare"
    , "Apple Days"
    , "33.859312, 35.488504  La Perla Building, GF, Embassies Street, Jnah "
    , "First Eco-Nursery in Lebanon, Certified since 2009. We pride ourselves on being a friendly Nursery, providing a home like home safe and loving space. \nWe have a wonderful team of teachers, nurses, & therapists with 20 years of expertise in serving the early years.\nWe apply high educational and health standards to help raising in partnership with parents a happy child.\nWe include all children in active learning by adapting to meet their needs, and to support each child’s individual needs and prepare them to school."
    , "Monday till Friday 7 30 AM till 2 30 PM\n Monday till Friday 7 30 AM till 5 PM"
    , "   0-1|1-4 "
    , "4 months to 4 years"
    , "$$"
    , "$550"
    , " 76653844/70975965"
    , " info@appledaysnursery.com"
    , "www.appledaysnursery.com/\n\r www.facebook.com/appledaysnursery \nwww.instagram.com/apple.days"
    , "toilets, kitchen, changing tables, outdoor playground, activity area, bus service"
    , "yes"
    , "Montessori inspired, Languages: English & French"
    , "requires booking, camps, application"
    ] &> trim
