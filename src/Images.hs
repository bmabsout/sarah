{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Images(module Images) where

import Prelude hiding (String)
import Foundation(String)
import qualified Foundation
import qualified Foundation.String as Foundation
import qualified Data.Map as M
import qualified Lib
import qualified Data.Text as T
import Data.FuzzySet
import Data.Vinyl
import Convenience
import Control.Applicative
import System.Directory
import System.FilePath
import Data.List
import FeaturesHandler (toFuzzy)


data Images' a = Images {
    name :: a,
    featured :: a,
    background :: a,
    gallery :: [a]
} deriving (Show, Eq, Functor)

type Images = Images' String

instance Monoid a => Monoid (Images' a) where
    mempty = Images mempty mempty mempty mempty

instance Semigroup a => Semigroup (Images' a) where
    Images a1 b1 c1 d1 <> Images a2 b2 c2 d2 = Images (a1<>a2) (b1<>b2) (c1<>c2) (d1 <> d2)

type ListingsToImages = M.Map String Images

flistDirectory s = listDirectory s &>> Foundation.fromList

addFilePath :: FilePath -> Foundation.String -> Foundation.String
addFilePath path s = Foundation.replace "\\" "/" (Foundation.fromList $ path </> Foundation.toList s)

readImages :: FilePath -> String -> IO (Either String Images)
readImages filePath listing = do
    dirItems <- flistDirectory filePath &>> Foundation.fromList
    let fileExists :: String -> Either String String
        fileExists fileName =
            dirItems
            & find (\file -> Foundation.lower fileName `Foundation.isPrefixOf` Foundation.lower file)
            & maybe (Left $ "the file:" <> fileName <> " is missing inside: " <> listing) (addFilePath filePath &. Right)

    let gallery = fileExists "gallery"
    galleryPics <- gallery
                   &> (Foundation.toList &. flistDirectory)
                   & sequence



    pure $ Images <$> pure listing 
                  <*> fileExists "featured" 
                  <*> fileExists "background"
                  <*> ((\pics galleryPath ->
                           pics &> addFilePath (Foundation.toList galleryPath))
                      <$> galleryPics <*> gallery)

readAllImages :: IO ListingsToImages
readAllImages = do
    listings <- listDirectory "images" 
    images <- mapM (\listing -> readImages ("images"</>listing) (Foundation.fromList listing)) listings 
    case (sequence images) of
        Left e -> Foundation.putStrLn e >> pure mempty
        Right ims -> pure (ims &> (\i -> (name i, i)) & M.fromList)

wrongImagesHandler :: ListingsToImages -> [([String], Lib.Row)] -> Either String [([String], Lib.Row, Images)]
wrongImagesHandler images rows = rows &> wrongImageHandler & sequence
  where wrongImageHandler (cats,row) =
            case images M.!? Lib.name row of
                Just image ->  Right (cats,row,image)
                Nothing -> Left (Lib.name row <> " doesn't have a corresponding folder in images\n"
                                              <> "Closest I have is " <> Foundation.show closest)
          where closest = (images & M.keys & toFuzzy) `getOne` (Lib.name row & Foundation.toList & T.pack)