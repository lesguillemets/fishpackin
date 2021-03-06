{-# LANGUAGE RecordWildCards #-}
module Fish ( Fish (..)
            , Loc
            , FishKind
            , Tolerance
            , FishData
            , move
            ) where
import Data.Complex
import qualified Data.Map.Strict as M

import Consts

type Loc = Complex Double
type FishKind = Int
type Tolerance = Double
type FishData = M.Map (Int,Int) Tolerance

data Fish = Fish {_kind :: FishKind, _loc :: Loc} deriving (Eq, Show)

distSquare :: Fish -> Fish -> Double
distSquare (Fish {_loc=l0}) (Fish {_loc=l1}) =
        sq (realPart l0 - realPart l1) + sq (imagPart l0 - imagPart l1)

sq :: Num a => a -> a
sq = (^ (2::Int))

tolerance :: FishData -> Fish -> Fish -> Double
tolerance d f0 f1 = d M.! (_kind f0, _kind f1)

tooClose :: FishData -> Fish -> Fish -> Bool
tooClose d f0 f1 = distSquare f0 f1 < tolerance d f0 f1

move :: FishData -> [Fish] -> [(Fish, Bool)]
move dat fs = map moveOne fs where
    -- TODO : consider better implementation
    force :: Fish -> Fish -> Loc
    force f0 f1 = let
        v = _loc f0 - _loc f1 in
            (dt:+0) * ((tolerance dat f0 f1 :+ 0) - abs v) * signum v
    moveOne :: Fish -> (Fish, Bool)
    moveOne f@(Fish {..}) = let
        neighbours = filter (/= f) . filter (tooClose dat f) $ fs
        mean = (/ ((fromIntegral . length) neighbours :+ 0))
        totalForce = mean . sum . map (force f) $ neighbours
        in
            (f {_loc = _loc + totalForce}, toEnum . signum $ length neighbours)
