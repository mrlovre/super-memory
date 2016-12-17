module Process where

import           Control.Arrow
import           Control.Monad
import           Graphics.UI.Gtk
import           Variables

decimationFactor :: Int
decimationFactor = 10

normalizeData :: [Point] -> [DecimalPoint]
normalizeData points = let
    (m, n) = join (***) fromIntegral . (maximum &&& minimum) $ concatMap flattenTuple points
    flattenTuple (a, b) = [a, b]
    (meanX, meanY) = join (***) (/ fromIntegral (length points)) . join (***) fromIntegral $
            foldl (\ (a, b) (c, d) -> (a + c, b + d)) (0, 0) points
    in map (\ (x, y) -> ((fromIntegral x - meanX) / (m - n), (fromIntegral y - meanY) / (m - n))) points

decimateData :: [Point] -> [Point]
decimateData [] = []
decimateData points = let
    segmentLength = length points `div` pred decimationFactor
    in [points !! (i * segmentLength) | i <- [0 .. decimationFactor - 2]] ++ [last points]
