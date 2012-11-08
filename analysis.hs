import Text.TSV
import Text.CSV
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

loadClusters :: [[String]] -> [(String,Int)]
loadClusters = map (\s -> (head s, read . last $ s))

loadPaEntry :: [String] -> RawPaMeasurement
loadPaEntry line = RawPaMeasurement {
    pname = head line,
    mTime = read $ line !! 4,
    doublingTime = read $ line !! 9,
    pA = read $ line !! 10
}

paID :: RawPaMeasurement -> String
paID = head . tail . splitOn "__" . takeWhile (/= ';') . pname

paVals :: RawPaMeasurement -> PaVals
paVals rpm = PaVals {
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


main = do
    original_pa <- parseTSVFromFile pa_filename
    clusters <- parseCSVFromFile clusters_filename
    let original_pa_data =  either (error "failed to load pa file") loadPaData $ original_pa
    let clusters_data = either (error "failed to load clusters file") loadClusters $ clusters
    let no_hs = filter (not . isInfixOf "HS" . pname) . filter (isInfixOf "__" . pname) $ original_pa_data
    let pMap = foldl' (\m pa -> M.insertWith (++) (paID pa) (return $ paVals pa) m) M.empty no_hs
    putStrLn "first keys are:"
    mapM_ (putStrLn) . take 20 . M.keys $ pMap
    putStrLn "first clusters are:"
    mapM_ (putStrLn . show) . take 20 $ clusters_data
    putStrLn . show . length . M.keys $ pMap
    writeFigure PNG "test1.png" (1000,500) . makeFigure . head . drop 5 . M.toList $ pMap
    writeFigure PNG "test2.png" (1000,500) . makeFigure . head . drop 6 . M.toList $ pMap
    writeFigure PNG "test3.png" (1000,500) . makeFigure . head . drop 7 . M.toList $ pMap

