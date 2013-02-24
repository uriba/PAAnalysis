import Biolab.Interfaces.MySql
import Biolab.Analysis.GrowthRate
import Biolab.Analysis.Utils
import Biolab.Analysis.Normalization
import Biolab.Types
import Biolab.Analysis.Types
import qualified Data.Vector as V
import Data.Time (UTCTime, NominalDiffTime)
import Text.CSV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Vector ((!))
import Statistics.Sample (Sample, mean)
import Statistics.LinearRegression (linearRegression, nonRandomRobustFit, defaultEstimationParameters, EstimationParameters(..))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Statistics.Function (sortBy)
import Data.Function (on)
import Control.Arrow ((***))
import Biolab.Analysis.Types
import Biolab.Types
import Biolab.Analysis.Utils

listPairToList a b c i = pairToList (a !! i) ++ pairToList (b !! i) ++ pairToList (c !! i)

pairToList (a,b) = [a,b]

main = do
    absmes <- get_compare_data get_abs
    fl1mes <- get_compare_data get_fls
    fl2mes <- get_compare_data get_fls2

    writeFile "graph1.csv" $ printCSV $ listPairToList absmes fl1mes fl2mes 0
---------------------------------------
    writeFile "graph2.csv" $ printCSV $ listPairToList absmes fl1mes fl2mes 1
---------------------------------------
    writeFile "graph3.csv" $ printCSV $ listPairToList absmes fl1mes fl2mes 2
---------------------------------------
    writeFile "graph4.csv" $ printCSV $ listPairToList absmes fl1mes fl2mes 3
    return ()

get_compare_data :: ColonySample a => (Well -> IO (a RawMeasurement)) -> IO ([([String],[String])])
get_compare_data get_mes = do
    blk <- get_abs (Well 'e' 1) -- Leeat exp
    -- blk <- get_mes (Well 'h' 12)   -- Coli exp
    mes <- get_mes (Well 'd' 7)

    let threshold = thresholdFromBlank blk
    let normalized = normalizeFromInit threshold $ mes

    let dtss5 = V.map (\(x,Just y) -> (x,y)) . V.filter (not . (== Nothing) . snd) . modGrowthRate linearRegression 5 $ normalized
    let simp5 = (getTimes dtss5,map (show . (*3600)) . V.toList . V.map snd $ dtss5)

    let dtss15 = V.map (\(x,Just y) -> (x,y)) . V.filter (not . (== Nothing) . snd) . modGrowthRate linearRegression 15 $ normalized
    let simp15 = (getTimes dtss15,map (show . (*3600)) . V.toList . V.map snd $ dtss15)
    
    let dtsr6 = V.map (\(x,Just y) -> (x,y)) . V.filter (not . (== Nothing) . snd) . modGrowthRate (nonRandomRobustFit estimationParameters) 5 $ normalized
    let robust5 = (getTimes dtsr6,map (show . (*3600)) . V.toList . V.map snd $ dtsr6)

    let dtsr16 = V.map (\(x,Just y) -> (x,y)) . V.filter (not . (== Nothing) . snd) . modGrowthRate (nonRandomRobustFit estimationParameters) 15 $ normalized
    let robust15 = (getTimes dtsr16,map (show . (*3600)) . V.toList . V.map snd $ dtsr16)

    return [simp5,simp15,robust5,robust15]

estimationParameters = defaultEstimationParameters {outlierFraction = 0.25}

-- sqb w = SampleQuery ["2013-02-12 13:46:13"] [0] [w] -- Coli good exp
-- sqb w = SampleQuery ["2013-02-07 16:42:52"] [0] [w] -- Coli bad exp
sqb w = SampleQuery ["2013-01-06 12:00:00"] [1] [w] -- Leeat exp

get_abs :: Well -> IO (AbsorbanceSample RawMeasurement)
get_abs w = do
    db_conf <- dbConnectInfo "db.conf"
    mes <- loadMes db_conf (sqb w)
    return $ (\(AbsorbanceMeasurement x) -> x) . head . filter filterAbs . snd . head $ mes

get_fls :: Well -> IO (FluorescenseSample RawMeasurement)
get_fls w = do
    db_conf <- dbConnectInfo "db.conf"
    mes <- loadMes db_conf (sqb w)
    return $ (\(FluorescenseMeasurement x) -> x) . head . filter (not . filterAbs) . snd . head $ mes

get_fls2 :: Well -> IO (FluorescenseSample RawMeasurement)
get_fls2 w = do
    db_conf <- dbConnectInfo "db.conf"
    mes <- loadMes db_conf (sqb w)
    return $ (\(FluorescenseMeasurement x) -> x) . head . tail . filter (not . filterAbs) . snd . head $ mes

getTimes :: V.Vector (NominalDiffTime,a) -> [String]
getTimes ca = map ( show . (/3600) . realToFrac ) . V.toList . V.map fst $ ca

filterAbs (AbsorbanceMeasurement _) = True
filterAbs _ = False

modGrowthRate :: (ColonySample a) => (U.Vector Double -> U.Vector Double -> (Double,Double)) -> Int -> a NormalizedMeasurement -> V.Vector (NominalDiffTime,Maybe Double)
modGrowthRate fit window_size mes = V.fromList . map (modGrowthRateWindow fit . V.take window_size) . takeWhile (\x -> V.length x >= window_size) . iterate V.tail . V.map (realToFrac *** (logBase 2 . nmVal)) . trim $ dmes
    where
        (start,dmes) = absoluteToRelativeTime . measurements $ mes

modGrowthRateWindow :: (U.Vector Double -> U.Vector Double -> (Double,Double)) -> V.Vector (Double,Double) -> (NominalDiffTime,Maybe Double)
modGrowthRateWindow fit v = (realToFrac . fst $ v ! 0 {-(window_size `div` 2) -},modCalcGrowthRate fit v)

modCalcGrowthRate :: (U.Vector Double -> U.Vector Double -> (Double,Double)) -> V.Vector (Double,Double) -> Maybe Double
modCalcGrowthRate fit xys = gr . snd . fit xs $ ys
    where
        (xs,ys) = U.unzip . U.fromList . V.toList $ xys
        gr x = if x <=0 then Nothing else Just x

