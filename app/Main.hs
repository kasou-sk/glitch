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
import Data.List
import Data.Tuple.Curry
import Control.Lens hiding ((.=))

data PathVertex = PathVertex { _ts :: Int
                             , _lon :: Double
                             , _lat :: Double
                             , _alt :: Double
                             } deriving (Generic, Show)
makeLenses ''PathVertex

instance ToJSON PathVertex where
    --toEncoding = genericToEncoding defaultOptions
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

data InnerItem a = InnerItem { _prev :: a
                             , _self :: a
                             , _next :: a
                             } deriving (Show)
makeLenses ''InnerItem

--data TransitionEdge a b = TransitionEdge { _item1 :: a
--                                         , _item2 :: a
--                                         , _props :: b
--                                         }
--makeLenses ''TransitionEdge

asInnerItems :: [a] -> Either [a] [InnerItem a]
asInnerItems l1@(_:l2@(_:l3@(_:_))) = Right $ zipWith3 (InnerItem) l1 l2 l3
asInnerItems xs = Left xs

splitToSegments :: [PathVertex] -> [[PathVertex]]
splitToSegments [] = []
splitToSegments xs = ys : splitToSegments zs
    where (ys,zs) = continuousSpan isTogether xs    
          isTogether vx1 vx2 = vx2^.ts - vx1^.ts < 60 && distance vx1 vx2 > 0

continuousSpan :: (a -> a -> Bool) -> [a] -> ([a],[a])
continuousSpan p (x:xs@(y:_))
    | p x y     = let (ys,zs) = continuousSpan p xs in (x:ys,zs)
    | otherwise = ([x],xs)
continuousSpan _ xs = (xs,[])

middlePoint :: PathVertex -> PathVertex -> Int -> PathVertex
middlePoint vx1 vx3 ts2 = PathVertex { _ts = ts2
                                     , _lon = middleVal lon
                                     , _lat = middleVal lat
                                     , _alt = middleVal alt
                                     }
    where dts13 = fromIntegral $ vx3^.ts - vx1^.ts
          dts12 = fromIntegral $ ts2 - vx1^.ts
          dts23 = fromIntegral $ vx3^.ts - ts2
          middleVal val = (dts23 * vx1^.val + dts12 * vx3^.val) / dts13


deviation :: InnerItem PathVertex -> Double
deviation iv = distance vxSelf vxM
    where vxSelf = iv^.self
          vxPrev = iv^.prev
          vxNext = iv^.next
          tsSelf = vxSelf^.ts
          vxM = middlePoint vxPrev vxNext tsSelf

angleCos :: InnerItem PathVertex -> Double
angleCos iv = sphericalAngleCos a b c
    where a = angularDistance vxPrev vxSelf
          b = angularDistance vxSelf vxNext
          c = angularDistance vxPrev vxNext
          vxPrev = iv^.prev
          vxSelf = iv^.self
          vxNext = iv^.next


speed :: InnerItem PathVertex -> Double
speed iv = (distance vxPrev vxSelf + distance vxSelf vxNext) / fromIntegral (vxNext^.ts - vxPrev^.ts)
    where vxSelf = iv^.self
          vxPrev = iv^.prev
          vxNext = iv^.next

toTuple :: InnerItem PathVertex -> (Int, Double, Double, Double)
toTuple iv = (iv^.self.ts, deviation iv, angleCos iv, speed iv)

asPrintedLines :: Show a => [a] -> [C.ByteString]
asPrintedLines = fmap $ C.pack . show

isGlitch :: InnerItem PathVertex -> Bool
isGlitch iv = glitchFactor iv > 0.1

glitchFactor :: InnerItem PathVertex -> Double
glitchFactor iv = d*(1+a)*(1+a)/sqrt(s)
    where d = deviation iv
          a = angleCos iv
          s = speed iv

avg :: (a -> Double) -> [a] -> Double
avg f xs = avg' 0 0 xs
    where avg' s c [] = s / fromIntegral c
          avg' s c (x:xs) = avg' (s + f x) (c + 1) xs

dropGlitches :: [InnerItem PathVertex] -> [InnerItem PathVertex]
dropGlitches (iv1:l2@(iv2:iv3:iv4:ivs))
    | isGlitch iv2 && isGlitch iv3 = if notDroppingAny then iv1 : dropGlitches l2
                                                       else dropGlitches oneLess
    where oneLess = if betterToDrop3rd
                    then iv1:iv2':iv4':ivs
                    else iv1':iv3':iv4:ivs
          notDroppingAny = currentGFAvg < min dropped2ndGFAvg dropped3rdGFAvg
          betterToDrop3rd = dropped2ndGFAvg  > dropped3rdGFAvg
          currentGFAvg = avg glitchFactor [iv1,iv2,iv3,iv4]
          dropped2ndGFAvg = avg glitchFactor [iv1',iv3',iv4]
          dropped3rdGFAvg = avg glitchFactor [iv1,iv2',iv4']
          iv1' = iv1&next .~ iv3^.self
          iv3' = iv3&prev .~ iv1^.self
          iv2' = iv2&next .~ iv4^.self
          iv4' = iv4&prev .~ iv2^.self

dropGlitches (iv1:l2@(iv2:iv3:ivs))
    | isGlitch iv2 = if notDropping then iv1 : dropGlitches l2
                                    else dropGlitches oneLess
    where oneLess = iv1':iv3':ivs
          notDropping = currentGFAvg < dropped2ndGFAvg 
          currentGFAvg = avg glitchFactor [iv1,iv2,iv3]
          dropped2ndGFAvg = avg glitchFactor [iv1',iv3']
          iv1' = iv1&next .~ iv3^.self
          iv3' = iv3&prev .~ iv1^.self

dropGlitches (iv1:ivs) = iv1 : dropGlitches ivs
dropGlitches [] = []

skipEmpty toDo [] = []
skipEmpty toDo xs = toDo xs

ivToJsLine :: PathVertex -> C.ByteString
ivToJsLine vx = C.pack $ "{ts:" ++ (show $ vx^.ts) ++ ",lat:" ++ (show $ vx^.lat) ++ ",lng:" ++ (show $ vx^.lon) ++ "},"

devNull _ = Nothing

main :: IO ()
main = C.interact $
    C.lines >>>
    linesAsVertices >>>
    splitToSegments >>>
    fmap (
        asInnerItems >>>
        either
            devNull
            (
                dropGlitches >>>
                --map toTuple >>>
                --map (\(t,d,a,s) -> (t,d,a,s,d*(1+a)*(1+a)/sqrt(s))) >>>
                --filter (\(_,_,_,_,e) -> e > 0.1) >>>
                Just
            )
    ) >>>
    catMaybes >>>
    concatMap (
        map (ivToJsLine . _self) >>>
        ("addPolyLine([" :) >>>
        (++ ["]);"])
    ) >>>
    C.unlines

