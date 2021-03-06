{-# LANGUAGE FlexibleContexts #-}
import Text.TSV
import Text.CSV
import Safe (readMay, atMay, headMay)
import Data.Either (either)
import qualified Data.Map as M
import Data.List (foldl', isInfixOf, sortBy, group, nub, genericLength, deleteFirstsBy)
import Data.List.Split
import qualified Data.Packed.Vector as V
import Graphics.Rendering.Plot
import Statistics.LinearRegression
import qualified Statistics.Sample as S
import Statistics.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as B
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad ((<=<))
import Data.Function (on)
import qualified Data.Foldable as F

mean :: (F.Foldable t) => t Double -> Double
mean = fini . F.foldl' go (0, 0)
  where
    fini (a, _) = a
    go (m, n) x = (m', n')
        where m' = m + (x - m) / fromIntegral n'
              n' = n + 1

data RawPaMeasurement = RawPaMeasurement {
    pname :: String,
    mTime :: Integer,
    doublingTime :: Double,
    pA :: Double
    } deriving Show

data PaVals = PaVals {
    pvTime :: Integer,
    pvMed :: String,
    pvGr :: Double,
    pvPa :: Double
    } deriving (Show, Eq)

data CalibData = CalibData {
    cdMed :: String,
    cdGr :: Double,
    cdPa :: Double
    } deriving (Show,Eq)

loadPaData :: [[String]] -> [RawPaMeasurement]
loadPaData = tail . catMaybes . map loadPaEntry

loadClusters :: [[String]] -> [(String,Int)]
loadClusters = map (\s -> (head s, read . last $ s))

loadCalibData :: [[String]] -> [CalibData]
loadCalibData = map lcd
    where
        lcd line = CalibData {
            cdMed = med . head $ line,
            cdGr = 3600/(read . last $ line),
            cdPa = read (line !! 1)
    }

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
        pvTime = mTime rpm,
        pvMed = med . pname $ rpm,
        pvGr = 3600/doublingTime rpm,
        pvPa = pA rpm
        }

med :: String -> String
med = takeWhile (/= ';') . last . splitOn "__"

pa_filename = "DATE_GR_PA_PAnewWindow.tab"
outliers_filename = "plate_detected_outliers.tab"
clusters_filename = "Clusters.csv"
calib_filename = "calib_data.csv"
calib_gene = "YOR063W"

paGrXs :: [PaVals] -> V.Vector Double
paGrXs = V.fromList . map pvGr

paGr2Xs :: [PaVals] -> V.Vector Double
paGr2Xs = V.mapVector (\x -> x^2) . paGrXs

series :: V.Vector Double -> V.Vector Double -> (Double,[FormattedSeries])
series xs ys = (r2,[ point ys Cross,
                     line ((\x -> alpha1 + beta1 * x) :: Function) ( 1.0 :: LineWidth),
                     line ((\x -> beta * x) :: Function) ( 1.0 :: LineWidth)
                     ])
    where
        (alpha1,beta1) = linearRegressionTLS (fl xs) (fl ys)
        fl x = U.fromList . V.toList $ x :: Sample
        r2 = (correl  (fl xs) (fl ys)) ^ 2
        beta = linearRegressionTLS00 (U.fromList . V.toList $ xs :: Sample) (U.fromList . V.toList $ ys :: Sample)

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

makeCDSeries :: [CalibData] -> [PaVals] -> String -> (Series,Series)
makeCDSeries cd pv m = (V.fromList times, V.fromList . map scale $ times)
    where
        rpv = filter (\x -> pvMed x == m) pv
        rcd = filter (\x -> cdMed x == m) cd
        times = nub . map (fromIntegral . pvTime) $ rpv
        mes_t t = filter (\x -> (fromIntegral . pvTime $ x) == t) $ rpv
        scale t = (mean . map (\x -> pvPa x / ((pvGr x)^2)) $ mes_t t)/avg_calib_pa
        avg_calib_pa = mean . map (\x -> cdPa x / (cdGr x ^ 2)) $ rcd

normalizationData :: [CalibData] -> [PaVals] -> Figure ()
normalizationData cd pv = do
    withTitle . setText $ "Normalization data"
    setPlots 1 1
    let media = nub . map cdMed $ cd
    let calib_series = map (makeCDSeries cd pv) $ media
    let dataset = map (\(a,b) -> (Point,a,b)) calib_series
    withPlot (1,1) $ do
        addAxis XAxis (Side Lower) $ do
            setTicks Minor (Left 5)
            withAxisLine $ do
                setLineWidth 1.0
        addAxis YAxis (Side Lower) $ do
            setTicks Minor (Left 5)
            withAxisLabel . setText $ "relativePA"
            withAxisLine $ do
                setLineWidth 1.0
        setDataset dataset
        setRangeFromData XAxis Lower Linear
        setRangeFromData YAxis Lower Linear
        setLegend True NorthEast Inside
    
linearRegressionTLS00 :: S.Sample -> S.Sample -> Double
linearRegressionTLS00 xs ys = beta
    where
          sqr zs               = U.sum (U.map (^2) zs) / (n-1)
          !n                   = fromIntegral $ U.length xs
          !c                   = U.sum (U.zipWith (*) xs ys) / (n-1)
          !b                   = (sqr xs - (sqr ys)) / c
          !m1                  = S.mean xs 
          !m2                  = S.mean ys
          !betas               = [(-b - sqrt(b^2+4))/2,(-b + sqrt(b^2+4)) /2]
          !beta                = if c > 0 then maximum betas else minimum betas

main = do
    original_pa <- parseTSVFromFile pa_filename
    original_outliers <- parseTSVFromFile outliers_filename
    clusters <- parseCSVFromFile clusters_filename
    calib <- parseCSVFromFile calib_filename
    let original_pa_data =  either (error "failed to load pa file") id $ original_pa :: [[String]]
    let outliers_data =  either (error "failed to load outliers file") id $ original_outliers :: [[String]]
    let relevant_pa = loadPaData . deleteFirstsBy (\a b -> head a == head b) original_pa_data $ outliers_data
    let clusters_data = either (error "failed to load clusters file") loadClusters $ clusters
    let calib_data = either (error "failed to load calibration data") (loadCalibData  . init) $ calib
    let no_hs = filter (not . isInfixOf "HS" . pname) . filter (isInfixOf "__" . pname) $ relevant_pa 
    let pMap = M.filter (not . null) $ foldl' (\m pa -> M.insertWith (++) (paID pa) (maybeToList $ paVals pa) m) M.empty no_hs
    putStrLn "processing pa file..."
    putStrLn "processing clusters file..."
    putStrLn "total promoters loaded:"
    putStrLn . show . length . M.keys $ pMap
    mapM_ (putStrLn . show) $ calib_data

    writeFigure PNG ("normalization.png") (800,800) . normalizationData calib_data $ pMap M.! calib_gene
    mapM_ makeFigureFile . M.toList $ pMap

