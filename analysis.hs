import Text.TSV
import Data.Either (either)

data RawPaMeasurement = RawPaMeasurement {
    pname :: String,
    mTime :: Integer,
    doublingTime :: Double,
    pA :: Double
    } deriving Show

loadPaData :: [[String]] -> [RawPaMeasurement]
loadPaData = map loadPaEntry

loadPaEntry :: [String] -> RawPaMeasurement
loadPaEntry line = RawPaMeasurement {
    pname = head line,
    mTime = read $ line !! 4,
    doublingTime = read $ line !! 9,
    pA = read $ line !! 10
}

filename = "DATE_GR_PA_PAnewWindow.tab"
main = do
    original <- parseTSVFromFile filename
    let original_data =  either (error "failed to load file") (loadPaData . tail) $ original
    mapM_ (putStrLn . show) . take 10 $ original_data
