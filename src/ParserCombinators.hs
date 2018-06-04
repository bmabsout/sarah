{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserCombinators(module ParserCombinators) where

import Control.Monad (join)
import Foundation
import Foundation.Parser
import Foundation.String
import Foundation.Collection
import Foundation.String.Read (readNatural)
import Data.Char (isDigit)
import Convenience


divideUsualStrings :: String -> Either String [String]
divideUsualStrings = parseAll (sepBy delimiter) &.> filter (not.null)

delimiter :: Parser String ()
delimiter = spaces *> regularDelimiters <* many' (space <|> newLine)

regularDelimiters = some' $ element '/' <|> element '\\' <|> element '|' <|> element ',' <|> newLine

many' p = const () <$> many p
some' p = const () <$> some p

space = element ' ' <|> element '\t'
spaces = many' space

newLine = element '\n' <|> element '\r'

sepBy :: Parser String () -> Parser String [String]
sepBy pattern =
    many (const Nothing <$> pattern <|> Just <$> anyElement)
    &>  splitOn (== Nothing)
    &.> catMaybes
    &.> fromList
    &. emptyCase
    where emptyCase [e]
              | null e = []
              | otherwise = [e]
          emptyCase s = s

trim :: String -> String
trim = span spaceCheck &. snd &. spanEnd spaceCheck &. fst

spaceCheck e = e == ' ' || e == '\t' || e == '\n' || e == '\r'

asIs :: String -> Either String String
asIs = Right

digit :: Parser String Char
digit = satisfy_ isDigit

digits :: Parser String Natural
digits = some digit
         &> fromList
         &> readNatural
         &> (\case Just n -> pure n
                   Nothing -> reportError $ Satisfy $ Just "not a number")
         & join

parseBool s = Right $ case lower s of
    "yes" -> True
    "no" -> False

divideAndParse :: Parser String () -> Parser String a -> String -> Either String [a]
divideAndParse delimiter parser s = parseAll (sepBy delimiter) s &>> parseAll parser &> sequenceA & join

divideUsual :: Parser String a -> String -> Either String [a]
divideUsual = divideAndParse delimiter

showError ::  ParseError String -> String
showError (NotEnough (CountOf sz)) = "NotEnough: missing " <> show sz <> " element(s)"
showError NotEnoughParseOnly       = "NotEnough, parse only"
showError (ExpectedElement ex re)  = "Expected '" <> fromList [ex] <> "' but received '" <> fromList [re] <> "'"
showError (Expected ex re)         = "Expected '" <> ex <> "' but received '" <> re <> "'"
showError (Satisfy Nothing)        = "Satisfy"
showError (Satisfy (Just s))       = "Satisfy: " <> s

parseAll :: Parser String a -> String -> Either String a
parseAll p s = parse p s & toError & first showError
  where
    toError :: Result String a -> Either (ParseError String) a
    toError (ParseFailed err) = Left err
    toError (ParseOk more ok)
        | more == "" = Right ok
        | otherwise  = Left $ Satisfy $ Just ("There's extra stuff: " <> more)
    toError (ParseMore m) = toError (m mempty)

parseError s = reportError $ Satisfy (Just s)
