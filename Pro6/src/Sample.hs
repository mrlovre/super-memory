module Sample where
    
import Utility


data Sample where
    Sample :: { sx :: Double, sy :: Double } -> Sample

deriving instance Eq Sample

instance Show Sample where
    show Sample { .. } = "(" ++ show sx ++ "," ++ show sy ++ ")"


type Output = Double


trainData :: [(Sample, Output)]
trainData = let
    samples = map (uncurry Sample) $ [-4.0 .. 4.0] `meshGrid` [-4.0 .. 4.0]
    outputs = map generativeFunction samples
    in samples `zip` outputs

generativeFunction :: Sample -> Output
generativeFunction Sample { .. } = ((sx - 1.0) ** 2.0 + (sy + 2.0) ** 2.0 - 5.0 * sx * sy + 3.0) * cos (sx / 5.0) ** 2.0
