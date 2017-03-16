{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Lib
import Text.Printf
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Arrow
import Data.Angle
import Data.Maybe

data PathVertex = PathVertex { ts :: Int
                             , lon :: Double
                             , lat :: Double
                             , alt :: Double
                             } deriving (Generic, Show)

instance ToJSON PathVertex where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PathVertex where

toRadians x = x/180*pi

haversine :: PathVertex -> PathVertex -> Double
haversine !start !end = let r = 6371000 :: Double
                            lat1 = toRadians $ lat start
                            lat2 = toRadians $ lat end
                            dlat = (/2) $ toRadians $ lat1 - lat2
                            dlon = (/2) $ toRadians $ lon start - lon end
                            sqr x = x * x
                            a = sqr (sin dlat) + cos lat1 * cos lat2 * sqr (sin dlon)
                            c = 2 * atan2 (sqrt a) (sqrt $ 1 - a)
                        in r * c

linesAsVertices :: [C.ByteString] -> [PathVertex]
linesAsVertices = fmap decode >>> catMaybes

doublets :: [a] -> [(a, a)]
doublets l1@(_:l2@(_:_)) = zip l1 l2
doublets _ = []

distances :: [PathVertex] -> [Double]
distances = doublets >>> fmap (uncurry haversine)

triplets :: [a] -> [(a, a, a)]
triplets l1@(_:l2@(_:l3@(_:_))) = zip3 l1 l2 l3
triplets _ = []

asPrintedLines :: Show a => [a] -> [C.ByteString]
asPrintedLines = fmap $ C.pack . show

main :: IO ()
main = C.interact $
    C.lines >>>
    linesAsVertices >>>
    distances >>>
    asPrintedLines >>>
    C.unlines
