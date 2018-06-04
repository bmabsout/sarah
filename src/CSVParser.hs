module CSVParser where

import Text.ParserCombinators.ReadP

-- csvFile :: ReadP [[String]]
-- csvFile = endBy line eol

-- line :: ReadP [String]
-- line = sepBy cell (char ',')

-- cell :: ReadP String
-- cell = quotedCell <++ munch (`notElem` (",\n\r" :: String))

-- quotedCell :: ReadP String
-- quotedCell = between (char '"') (char '"') (many quotedChar)

-- quotedChar :: ReadP Char
-- quotedChar =
--         satisfy (/= '"') +++ (string "\"\"" >> return '"')

-- parse = readP_to_S

-- eol :: ReadP String
-- eol =  (string "\n\r")
--        <++ (string "\r\n")
--        <++ (string "\n")
--        <++ (string "\r")

-- parseWith :: ReadP a -> String -> a
-- parseWith p s
--     = case [a | (a,t) <- parse p s, all isSpace t] of
--         [a] -> a
--         []  -> error "no parse"
--         _   -> error "ambiguous parse"

-- parseCSV input = parse csvFile input

-- parseFile :: FilePath -> IO [Either Row Error]
-- parseFile name =
--     do c <- readFile name
--        return $ toRows $ parseWith csvFile c

-- eatCSV = parseFile "C:\\Users\\iggybibi\\Downloads\\Copy of new(405).csv"



-- instance FromField SubCategories where
--     parseField s = pure (SubCategories $ [U.toString s =~ url])


-- eatCSV = do
--     csv <- B.readFile "C:\\Users\\iggybibi\\Downloads\\Copy of new(405).csv"
--     print (decode NoHeader csv :: Either String (V.Vector Row))

