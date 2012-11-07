import Text.TSV
import Data.Either (either)
import qualified Data.Map as M
import Data.List (foldl', isInfixOf)
import Data.List.Split

data RawPaMeasurement = RawPaMeasurement {
    pname :: String,
    mTime :: Integer,
    doublingTime :: Double,
    pA :: Double
    } deriving Show

data PaVals = PaVals {
    pvMed :: String,
    pvGr :: Double,
    pvPa :: Double
    }

loadPaData :: [[String]] -> [RawPaMeasurement]
loadPaData = tail . map loadPaEntry

loadPaEntry :: [String] -> RawPaMeasurement
loadPaEntry line = RawPaMeasurement {
    pname = head line,
    mTime = read $ line !! 4,
    doublingTime = read $ line !! 9,
    pA = read $ line !! 10
}

paID :: RawPaMeasurement -> String
paID = head . splitOn "__" . takeWhile (/= ';') . pname

paVals :: RawPaMeasurement -> PaVals
paVals rpm = PaVals {
    pvMed = med . pname $ rpm,
    pvGr = 1/doublingTime rpm,
    pvPa = pA rpm
    }

med :: String -> String
med = last . splitOn "__"

filename = "DATE_GR_PA_PAnewWindow.tab"
main = do
    original <- parseTSVFromFile filename
    let original_data =  either (error "failed to load file") loadPaData $ original
    let no_hs = filter (not . isInfixOf "HS" . pname) . filter (isInfixOf "__" . pname) $ original_data
    let pMap = foldl' (\m pa -> M.insertWith (++) (paID pa) (return $ paVals pa) m) M.empty no_hs
    mapM_ (putStrLn) . take 200 . M.keys $ pMap
    putStrLn . show . length . M.keys $ pMap
