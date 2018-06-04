{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( module Lib
    ) where

import qualified Data.Set as S
import Data.Semigroup
import Foundation as F hiding ((<>))
import Foundation.String
import Foundation.Parser as P
import Data.Char (isDigit)
import Convenience
import ParserCombinators
import Data.Traversable
import Control.Monad
import qualified Data.Text as T
import FeaturesHandler
import qualified Data.FuzzySet as F

data Location = Coords {lon:: (String,String) , lat :: (String,String), address :: String}
              | Address String
              deriving (Show, Eq)

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
         deriving (Show, Enum, Eq, Ord, Bounded)

data AmPm = Am | Pm deriving (Show, Eq)
data Time = Time {hour :: Natural, minute :: Natural, ampm :: AmPm} deriving (Show, Eq)
data RangeDayAndTime = RangeDayAndTime (Either Day (Range Day)) (Range Time) deriving (Show,Eq)
data PhoneNumber = PhoneNumber {countryCode :: String, areaCode :: String, number :: String, extension :: Maybe String} deriving (Show,Eq)
data Url = Url {subnet :: String, urlAddress :: String, rest :: String} deriving (Show,Eq)

type Range a = (Min a, Max a)
type AgeGroup = Maybe (Range Natural)

phoneToString :: PhoneNumber -> String
phoneToString p = "(+"<>countryCode p<>") "
                  <> (let c = areaCode p in if length c == 1 then "0" <> c else c)
                  <> "-"
                  <> number p
                  <> maybe "" (" ext " <>) (extension p)

data Row = Row
    { subCategories       :: [String]
    , name                :: String
    , location            :: Location
    , description         :: String
    , openingHoursAndDays :: [RangeDayAndTime]
    , ageGroup            :: AgeGroup
    , ageRange            :: String
    , priceSymbol         :: String
    , priceRange          :: String
    , phoneNumbers        :: [PhoneNumber]
    , emails              :: [String]
    , websites            :: [Url]
    , facilities          :: [String]
    , photos              :: Bool
    , academicNotes       :: String
    , additionalNotes     :: [String]
    } deriving Show

data Error = ParseError String | NotARow deriving (Show,Eq)

toRows :: (Tags,Features) -> [[String]] -> [Either Row Error]
toRows tf = fmap (toRow tf)

toRow :: (Tags,Features) -> [String] -> Either Row Error
toRow _ [] = Right NotARow
toRow tf row@(firstCol:_)
    | firstCol == "" || lower firstCol == "sub category" = Right NotARow
    | otherwise =
        let eitherFier :: Either (String,String) Row -> Either Row Error
            eitherFier (Left err) = err & show & ParseError & Right
            eitherFier (Right r) = Left r

            clean s = replace "_x000D_" "\n" s & trim
        in row & fixRowLength &> clean & parseRow tf & eitherFier

fixRowLength :: [String] -> [String]
fixRowLength l
    | size > 16 = F.take 16 l
    | size < 16 = l <> (replicate (fromMaybe (CountOf 0)  $ CountOf 16 - size) "" :: [String])
    | otherwise = l
    where size = length l

parseLocation :: String -> Either String Location
parseLocation = parseAll $
    Coords <$> parseDecimal <* delimitDecimals <*> parseDecimal <* optional delimitDecimals <*> takeAll
    <|> Address <$> takeAll
    where parseDecimal = (,) <$> (P.takeWhile isDigit <* element '.')
                             <*> P.takeWhile isDigit
          delimitDecimals = many' (regularDelimiters <|> space)

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
                &>> foldl' (Data.Semigroup.<>) Nothing
    where
        singularGroupParser :: Parser String AgeGroup
        singularGroupParser =
            (\min max -> Just (Min min, Max max))
                <$> digits <* spaces <* element '-' <* spaces
                <*> digits <* spaces
            <|> elements "adult" <* optional (element 's') <* spaces *> pure Nothing


parseWebsites :: String -> Either String [Url]
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
    (locC,numC) <- getLocationCode 8 2 <|> getLocationCode 7 1
    ext <- (Just <$> (some' space <* elements "ext" <* some' space *> some digit &> fromList))
           <|> pure Nothing
    return $ PhoneNumber countryC locC numC ext
    where numDigits from to = repeat (Between $ from `And` to) digit &> fromList


parseFromSet :: F.FuzzySet -> String -> Either String [String]
parseFromSet set s =
    divideUsualStrings s
    &>> (\matchWith -> fuzzyDecider (F.get set (stringToText matchWith)) matchWith)
    &> sequenceA
    & join
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



parseRow :: (Tags,Features) -> [String] -> Either (String,String) Row
parseRow (Tags tags, Features features)
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
        Row
        <$> (parseFromSet tags (upper subCategories) & addCaption "subCategories")
        <*> (asIs name & addCaption "name")
        <*> (parseLocation location & addCaption "location")
        <*> (asIs description & addCaption "description")
        <*> (divideUsual parseRangeDayAndTime (lower openingHoursAndDays) & addCaption "openingHoursAndDays")
        <*> (parseAgeGroup (lower ageGroup) & addCaption "ageGroup")
        <*> (asIs ageRange & addCaption "ageRange")
        <*> (asIs priceSymbol & addCaption "priceSymbol")
        <*> (asIs priceRange & addCaption "priceRange")
        <*> (divideUsual parsePhoneNumber (lower phoneNumbers) & addCaption ("phoneNumbers: " <> phoneNumbers))
        <*> (divideUsualStrings (lower emails) & addCaption "emails")
        <*> (parseWebsites (lower websites) & addCaption "websites")
        <*> (parseFromSet features (upper facilities) & addCaption "facilities")
        <*> (parseBool (lower photos) & addCaption "photos")
        <*> (asIs academicNotes & addCaption "academicNotes")
        <*> (parseFromSet tags (upper additionalNotes) & addCaption "additionalNotes")
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
