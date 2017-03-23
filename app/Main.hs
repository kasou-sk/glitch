{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Lens hiding ((.=))

data PathVertex = PathVertex { _ts :: Double
                             , _lon :: Double
                             , _lat :: Double
                             , _alt :: Double
                             } deriving (Generic, Show)
makeLenses ''PathVertex

instance ToJSON PathVertex where
    toEncoding = genericToEncoding defaultOptions
    toJSON PathVertex{..} = object [ "ts" .= _ts
                                   , "lon" .= _lon
                                   , "lat" .= _lat
                                   , "alt" .= _alt
                                   ]

instance FromJSON PathVertex where
    parseJSON (Object o) = PathVertex <$> o .: "ts"
                                      <*> o .: "lon"
                                      <*> o .: "lat"
                                      <*> o .: "alt"
    parseJSON _ = fail "Failed to parse pathvertex!"

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


angularDistance :: PathVertex -> PathVertex -> Double
angularDistance vx1 vx2 = 2 * asin (sqrt $ (hav dlat) + (cos lat1) * (cos lat2) * (hav dlon))
    where dlon = lon2 - lon1
          dlat = lat2 - lat1
          lat1 = toRadians $ vx1^.lat
          lat2 = toRadians $ vx2^.lat
          lon1 = toRadians $ vx1^.lon
          lon2 = toRadians $ vx2^.lon
          square x = x * x
          hav f = square $ sin $ f / 2

sphericalAngleCos :: Double -> Double -> Double -> Double
sphericalAngleCos a b c = max (-1) $ min 1 $ cos'
    where cos' = (cos c - cos a * cos b) / (sin a * sin b)

sphericalAngle :: Double -> Double -> Double -> Double
sphericalAngle a b c = acos $ sphericalAngleCos a b c


distance :: PathVertex -> PathVertex -> Double
distance vx1 vx2 = let r = 6371000 :: Double
                       lat1 = toRadians $ vx1^.lat
                       lat2 = toRadians $ vx2^.lat
                       lon1 = toRadians $ vx1^.lon
                       lon2 = toRadians $ vx2^.lon
                       dlat = lat2 - lat1
                       dlon = lon2 - lon1
                       square x = x * x
                       hav f = square $ sin $ f / 2
                   in 2 * r * asin (sqrt $ (hav dlat) + (cos lat1) * (cos lat2) * (hav dlon))

haversine :: PathVertex -> PathVertex -> Double
haversine start end = let r = 6371000 :: Double
                          lat1 = toRadians $ start^.lat
                          lat2 = toRadians $ end^.lat
                          dlat = toRadians $ end^.lat - start^.lat
                          dlon = toRadians $ end^.lon - start^.lon
                          square x = x * x
                          a = (square (sin $ 0.5 * dlat)) + (cos lat1) * (cos lat2) * (square (sin $ 0.5 * dlon))
                          c = 2 * (atan2 (sqrt a) (sqrt $ 1 - a))
                      in r * c

linesAsVertices :: [C.ByteString] -> [PathVertex]
linesAsVertices = fmap decode >>> catMaybes

--doublets :: [a] -> [(a, a)]
--doublets l1@(_:l2@(_:_)) = zip l1 l2
--doublets _ = []

--distances :: [PathVertex] -> [Double]
--distances = fmap (uncurry haversine) . doublets 

--triplets :: [a] -> [(a, a, a)]
--triplets l1@(_:l2@(_:l3@(_:_))) = zip3 l1 l2 l3
--triplets _ = []

data InnerItem a = InnerItem { _prev :: a
                             , _self :: a
                             , _next :: a
                             }
makeLenses ''InnerItem

--data TransitionEdge a b = TransitionEdge { _item1 :: a
--                                         , _item2 :: a
--                                         , _props :: b
--                                         }
--makeLenses ''TransitionEdge

asInnerItems :: [a] -> [InnerItem a]
asInnerItems l1@(_:l2@(_:l3@(_:_))) = zipWith3 (InnerItem) l1 l2 l3
asInnerItems _ = []

middlePoint :: PathVertex -> PathVertex -> Double -> PathVertex
middlePoint vx1 vx3 ts2 = PathVertex { _ts = ts2
                                     , _lon = middleVal lon
                                     , _lat = middleVal lat
                                     , _alt = middleVal alt
                                     }
    where dts13 = vx3^.ts - vx1^.ts
          dts12 = ts2 - vx1^.ts
          dts23 = vx3^.ts - ts2
          middleVal val = (dts23 * vx1^.val + dts12 * vx3^.val) / dts13


deviation :: InnerItem PathVertex -> Double
deviation ii = distance vxSelf vxM
    where vxSelf = ii^.self
          vxPrev = ii^.prev
          vxNext = ii^.next
          tsSelf = vxSelf^.ts
          vxM = middlePoint vxPrev vxNext tsSelf

angleCos :: InnerItem PathVertex -> Double
angleCos ii = sphericalAngleCos a b c
    where a = angularDistance vxPrev vxSelf
          b = angularDistance vxSelf vxNext
          c = angularDistance vxPrev vxNext
          vxPrev = ii^.prev
          vxSelf = ii^.self
          vxNext = ii^.next


speed :: InnerItem PathVertex -> Double
speed ii = (distance vxPrev vxSelf + distance vxSelf vxNext) / (vxNext^.ts - vxPrev^.ts)
    where vxSelf = ii^.self
          vxPrev = ii^.prev
          vxNext = ii^.next

toTuple :: InnerItem PathVertex -> (Int, Double, Double, Double)
toTuple ii = (tsInt, deviation ii, angleCos ii, speed ii)
    where tsInt = floor (ii^.self.ts)

asPrintedLines :: Show a => [a] -> [C.ByteString]
asPrintedLines = fmap $ C.pack . show

main :: IO ()
main = C.interact $
    C.lines >>>
    linesAsVertices >>>
    asInnerItems >>>
    map toTuple >>>
    map (\(t,d,a,s) -> (t,d,a,s,d*(1+a)*(1+a)/sqrt(s))) >>>
    filter (\(_,_,_,_,e) -> e > 0.1) >>>
    asPrintedLines >>>
    C.unlines

