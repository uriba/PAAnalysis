import Text.TSV
import Text.CSV
import Safe (readMay, atMay, headMay)
import Data.Either (either)
import qualified Data.Map as M
import Data.List (foldl', isInfixOf)
import Data.List.Split
import qualified Data.Packed.Vector as V
import Graphics.Rendering.Plot
import Statistics.LinearRegression
import Statistics.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as B
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad ((<=<))

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
    } deriving Show

loadPaData :: [[String]] -> [RawPaMeasurement]
loadPaData = tail . catMaybes . map loadPaEntry

loadClusters :: [[String]] -> [(String,Int)]
loadClusters = map (\s -> (head s, read . last $ s))

loadPaEntry :: [String] -> Maybe RawPaMeasurement
loadPaEntry line = do
    name <- headMay line
    time <- readMay <=< atMay line $ 4
    dt <- readMay <=< atMay line $ 9
    pa <- readMay <=< atMay line $ 10
    return RawPaMeasurement { pname = name, mTime = time, doublingTime = dt, pA = pa }

paID :: RawPaMeasurement -> String
paID = head . tail . splitOn "__" . takeWhile (/= ';') . pname

paVals :: RawPaMeasurement -> Maybe PaVals
paVals rpm 
    | isNaN (doublingTime rpm) || doublingTime rpm < 0 = Nothing
    | isNaN (pA rpm) || pA rpm < 0 = Nothing
    | 'Y' /= (head . paID $ rpm) = Nothing
    | otherwise = Just PaVals {
        pvMed = med . pname $ rpm,
        pvGr = 1/(doublingTime rpm/3600),
        pvPa = pA rpm
        }

med :: String -> String
med = last . splitOn "__"

pa_filename = "DATE_GR_PA_PAnewWindow.tab"
clusters_filename = "Clusters.csv"

paGrXs :: [PaVals] -> V.Vector Double
paGrXs = V.fromList . map pvGr

paGr2Xs :: [PaVals] -> V.Vector Double
paGr2Xs = V.mapVector (\x -> x^2) . paGrXs

series :: V.Vector Double -> V.Vector Double -> (Double,[FormattedSeries])
series xs ys = (r2,[point ys Cross, line ((\x -> alpha + beta * x) :: Function) (1.0 :: LineWidth)])
    where
        (alpha,beta,r2) = linearRegressionRSqr (U.fromList . V.toList $ xs :: Sample) (U.fromList . V.toList $ ys :: Sample)

makeFigure :: (String,[PaVals]) -> Figure ()
makeFigure (t,pvs) = do
    withTitle . setText $ t
    setPlots 1 2
    let ys = map pvPa $ pvs
    let vys = V.fromList ys 
    withPlots $ do
        setBorder True
        setRange YAxis Lower Linear 0 . maximum $ ys
        addAxis XAxis (Side Lower) $ do
            setTicks Minor (Left 5)
            withAxisLine $ do
                setLineWidth 1.0
        addAxis YAxis (Side Lower) $ do
            setTicks Minor (Left 5)
            withAxisLabel . setText $ "PA"
            withAxisLine $ do
                setLineWidth 1.0
    withPlot (1,1) $ do
        let xs = paGrXs pvs
        withHeading . setText $ ("R^2=" ++ (show . fst . series xs $ vys))
        setDataset (xs, snd . series xs $ vys)
        setRange XAxis Lower Linear 0 . maximum . V.toList $ xs
        withAxis XAxis (Side Lower) $ do
            withAxisLabel . setText $ "GR"
    withPlot (1,2) $ do
        let xs = paGr2Xs pvs
        withHeading . setText $ ("R^2=" ++ (show . fst . series xs $ vys))
        setDataset (xs, snd . series xs $ vys)
        setRange XAxis Lower Linear 0 . maximum .  V.toList $ xs
        withAxis XAxis (Side Lower) $ do
            withAxisLabel . setText $ "GR^2"

makeFigureFile :: (String,[PaVals]) -> IO ()
makeFigureFile (name,vals) = do
    putStrLn $ "processing " ++ name
    writeFigure PNG ("output/" ++ name ++ ".png") (1000,500) . makeFigure $ (name,vals)

main = do
    original_pa <- parseTSVFromFile pa_filename
    clusters <- parseCSVFromFile clusters_filename
    let original_pa_data =  either (error "failed to load pa file") loadPaData $ original_pa
    let clusters_data = either (error "failed to load clusters file") loadClusters $ clusters
    let no_hs = filter (not . isInfixOf "HS" . pname) . filter (isInfixOf "__" . pname) $ original_pa_data
    let pMap = M.filter (not . null) $ foldl' (\m pa -> M.insertWith (++) (paID pa) (maybeToList $ paVals pa) m) M.empty no_hs
    putStrLn "processing pa file..."
    putStrLn "processing clusters file..."
    putStrLn "total promoters loaded:"
    putStrLn . show . length . M.keys $ pMap
    mapM_ makeFigureFile . M.toList $ pMap

