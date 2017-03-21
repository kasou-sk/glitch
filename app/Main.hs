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
--import Data.Angle
import Data.Maybe
import Data.Tuple.Curry
import Control.Lens

data PathVertex = PathVertex { ts :: Double
                             , lon :: Double
                             , lat :: Double
                             , alt :: Double
                             } deriving (Generic, Show)

instance ToJSON PathVertex where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PathVertex where

toRadians d = d/180*pi
toDegrees r = r/pi*180

--dIstance :: PathVertex -> PathVertex -> Double
--dIstance vx1 vx2 = let lat1 = toRadians $ lat vx1
--                       lat2 = toRadians $ lat vx2
--                       lon1 = toRadians $ lon vx1
--                       lon2 = toRadians $ lon vx2
--                       dlat = lat2 - lat1
--                       dlon = lon2 - lon1
--                       square x = x * x
--                       x = dlon * cos((lat1+lat2)/2)
--                       y = dlat
--                   in 6371000 * sqrt(x*x + y*y);

--angDistance :: PathVertex -> PathVertex -> Double
--angDistance vx1 vx2 = acos $ sin f1 * sin f2 + cos f1 * cos f2 + cos dl
--    where f1 = toRadians $ lat vx1
--          f2 = toRadians $ lat vx2
--          dl = toRadians $ lon vx2 - lon vx1


angDistance :: PathVertex -> PathVertex -> Double
angDistance vx1 vx2 = 2 * asin (sqrt $ (hav dlat) + (cos lat1) * (cos lat2) * (hav dlon))
    where dlon = lon2 - lon1
          dlat = lat2 - lat1
          lat1 = toRadians $ lat vx1
          lat2 = toRadians $ lat vx2
          lon1 = toRadians $ lon vx1
          lon2 = toRadians $ lon vx2
          square x = x * x
          hav f = square $ sin $ f / 2

sphericalAngle :: Double -> Double -> Double -> Double
sphericalAngle a b c = acos((cos c - cos a * cos b) / (sin a * sin b))


distance :: PathVertex -> PathVertex -> Double
distance vx1 vx2 = let r = 6371000 :: Double
                       lat1 = toRadians $ lat vx1
                       lat2 = toRadians $ lat vx2
                       lon1 = toRadians $ lon vx1
                       lon2 = toRadians $ lon vx2
                       dlat = lat2 - lat1
                       dlon = lon2 - lon1
                       square x = x * x
                       hav f = square $ sin $ f / 2
                   in 2 * r * asin (sqrt $ (hav dlat) + (cos lat1) * (cos lat2) * (hav dlon))

haversine :: PathVertex -> PathVertex -> Double
haversine start end = let r = 6371000 :: Double
                          lat1 = toRadians $ lat start
                          lat2 = toRadians $ lat end
                          dlat = toRadians $ lat end - lat start
                          dlon = toRadians $ lon end - lon start
                          square x = x * x
                          a = (square (sin $ 0.5 * dlat)) + (cos lat1) * (cos lat2) * (square (sin $ 0.5 * dlon))
                          c = 2 * (atan2 (sqrt a) (sqrt $ 1 - a))
                      in r * c

linesAsVertices :: [C.ByteString] -> [PathVertex]
linesAsVertices = fmap decode >>> catMaybes

doublets :: [a] -> [(a, a)]
doublets l1@(_:l2@(_:_)) = zip l1 l2
doublets _ = []

distances :: [PathVertex] -> [Double]
distances = fmap (uncurry haversine) . doublets 

triplets :: [a] -> [(a, a, a)]
triplets l1@(_:l2@(_:l3@(_:_))) = zip3 l1 l2 l3
triplets _ = []

middlePoint :: PathVertex -> PathVertex -> Double -> PathVertex
middlePoint vx1 vx3 ts2 = PathVertex { ts = ts2
                                     , lon = middleVal lon
                                     , lat = middleVal lat
                                     , alt = middleVal alt
                                     }
    where dts13 = ts vx3 - ts vx1
          dts12 = ts2 - ts vx1
          dts23 = ts vx3 - ts2
          middleVal val = (dts23 * val vx1 + dts12 * val vx3) / dts13


deviation:: PathVertex -> PathVertex -> PathVertex -> Double
deviation vx1 vx2 vx3 = distance vx1 vxM
    where ts2 = ts vx2
          vxM = middlePoint vx1 vx3 ts2

deviationTuples :: [PathVertex] -> [(Int, Double)]
deviationTuples vxs = zipWith toTuple vxs3 deviations
    where vxs3 = triplets vxs
          deviations = uncurryN deviation `fmap` vxs3
          toTuple (_, vx2, _) dev = (floor $ ts vx2, dev)

speedTuples:: [PathVertex] -> [((Int, Int), Double)]
speedTuples = fmap (\(vx1, vx2) -> ((floor $ ts vx1, floor $ ts vx2), haversine vx1 vx2 / (ts vx2 - ts vx1))) . doublets 

filterBySndLarger:: Double -> [(a, Double)] -> [(a, Double)]
filterBySndLarger trsh = filter p 
    where p (_, d) = d > trsh

asPrintedLines :: Show a => [a] -> [C.ByteString]
asPrintedLines = fmap $ C.pack . show

main :: IO ()
main = C.interact $
    C.lines >>>
    linesAsVertices >>>
    deviationTuples >>>
    filterBySndLarger 1.0 >>>
    --speedTuples >>>
    --filterBySndLarger 30.0 >>>
    asPrintedLines >>>
    C.unlines

