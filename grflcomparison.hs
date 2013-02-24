import Biolab.Interfaces.MySql
import Biolab.Analysis.GrowthRate
import Biolab.Analysis.Utils
import Biolab.Analysis.Normalization
import Biolab.Types
import Biolab.Analysis.Types
import Data.Map ((!))
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Time (UTCTime, NominalDiffTime)
import qualified Data.Packed.Vector as U
import Graphics.Rendering.Plot

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd3 (a,b,c) = c

allPlate :: [Well]
allPlate = concat $ zipWith (map) (map Well ['a' .. 'h']) (repeat [1..12])

dataToUse = (blank_wells,wells,experiments,plates)
    where
        blank_wells = [Well 'g' 12, Well 'h' 12]
        wells = allPlate -- blank_wells ++ filter ((/= 'e') . wRow) allPlate
        experiments = ["2013-02-12 13:46:13"]
        plates = [0]

main = do
    db_conf <- dbConnectInfo "db.conf"
    let (blank_wells,wells,experiments,plates) = dataToUse
    let sq = SampleQuery experiments plates wells
    mes <- loadMes db_conf sq
    let blanks = filter ((`elem` blank_wells) .  sidWell . fst) mes
    let blankods = getAbsorbance blanks
    let blankvals = map (backgroundFromBlank . snd) blankods
    let thresholdvals = map (thresholdFromBlank . snd) blankods
    mapM (putStrLn . show) blankvals
    mapM (putStrLn . show) thresholdvals
    let real_mes = filter (not . (`elem` blank_wells) . sidWell . fst) mes
    let rgods = getGrs (getAbsorbance blanks) (getAbsorbance real_mes)
    let rgfls = getGrs (getFluorescence blanks) (getFluorescence real_mes)
    let rgfl2s = getGrs (getFluorescence2 blanks) (getFluorescence2 real_mes)
    let res = zipWith3 (\a b c -> if (fst a == fst b && fst a == fst c)
        then (fst a,(
            [mvToV . fst3 . snd $ a, mvToV . fst3 . snd $ b, mvToV . fst3 . snd $ c],
            [rmToV . snd3 . snd $ a, rmToV . snd3 . snd $ b, rmToV . snd3 . snd $ c],
            [nmToV . trd3 . snd $ a, nmToV . trd3 . snd $ b, nmToV . trd3 . snd $ c]
            ))
        else error $ "colony id mismatch:" ++ show (fst a) ++ show (fst b) ++ show (fst c)) rgods rgfls rgfl2s
    let real_res = filter (not . null . catMaybes . fst3 . snd) res
    let figs = map (\(sid,(dts,raws,norms)) -> (sid,generateFigure (dts,raws,norms))) real_res
    mapM (\(sid,f) -> writeFigure PNG (makefn sid ++ ".png") (2400,800) f) figs
    return ()

makefn :: SampleId -> String
makefn sid = (show . sidPlate $ sid) ++ "-" ++ (show . wRow . sidWell $ sid) ++ (show . wColumn . sidWell $ sid)

sideof v = if (V.maximum . V.map snd $ v) < 3 then Lower else Upper

generateFigure :: ([Maybe (V.Vector (Double,Double))],[V.Vector (Double,Double)],[V.Vector (Double,Double)]) -> Figure ()
generateFigure (dts,raws,normed) = do
    setPlots 1 3
    -- have one quarter present raw od, the other raw fluorescence, next log-normalized, next comparison.
    withPlots $ do
        setBorder True
        addAxis YAxis (Side Lower) $ do
            setTicks Major (TickNumber 5)
            withAxisLine $ do
                setLineWidth 1.0
        addAxis XAxis (Side Lower) $ do
            setTicks Major (TickNumber 5)
            withAxisLine $ do
                setLineWidth 1.0
        setRange XAxis Lower Linear 0 44
    withPlot (1,3) $ do
        addAxis YAxis (Side Upper) $ do
            setTicks Major (TickNumber 5)
            withAxisLine $ do
                setLineWidth 1.0
        setDataset . map (\v -> (Line,
                                 U.fromList . V.toList . fst . V.unzip $ v,
                                 U.fromList . V.toList . snd . V.unzip $ v)) $ normed
        setRange YAxis Lower Linear (-10.0) 17
    withPlot (1,2) $ do
        addAxis YAxis (Side Upper) $ do
            setTicks Major (TickNumber 5)
            withAxisLine $ do
                setLineWidth 1.0
        setDataset . map (\v -> (Line,
                                 U.fromList . V.toList . fst . V.unzip $ v,
                                 (U.fromList . V.toList . snd . V.unzip $ v, sideof v))) $ raws
        setRange YAxis Lower Linear 0 2
        setRange YAxis Upper Linear 0 70000
    withPlot (1,1) $ do
        setDataset . map (\v -> (Line, U.fromList . V.toList . fst . V.unzip $ v, U.fromList . V.toList . snd . V.unzip $ v)) . catMaybes $ dts
        setRange YAxis Lower Linear 0 4
    withTitle (setText "comparison of growth rates based on OD and fluorescence")


mvToV :: V.Vector (NominalDiffTime, Maybe NominalDiffTime) -> Maybe (V.Vector (Double,Double))
mvToV v
    | V.null real_vals= Nothing
    | otherwise = Just $ V.map (\(x,y) -> ((/3600) . realToFrac $ x, (/3600) . realToFrac . fromJust $ y)) real_vals
    where real_vals = V.filter (isJust . snd) v

rmToV :: (ColonySample a) => a RawMeasurement -> V.Vector (Double,Double)
rmToV = V.map (\(x,y) -> ((/3600) . realToFrac $ x,mVal y)) . trim . snd . absoluteToRelativeTime . measurements

nmToV :: (ColonySample a) => a NormalizedMeasurement -> V.Vector (Double,Double)
nmToV = V.map (\(x,y) -> ((/3600) . realToFrac $ x,logBase 2 . nmVal $ y)) . trim . snd . absoluteToRelativeTime . measurements

getGrs :: (ColonySample a) => [(SampleId, a RawMeasurement)] -> [(SampleId, a RawMeasurement)] -> [(SampleId, (V.Vector (NominalDiffTime,Maybe NominalDiffTime), a RawMeasurement, a NormalizedMeasurement))]
getGrs blanks cultures = map (\(sid,v) -> (sid, (doublingTime (Just 9) {- Nothing -} . normed sid $ v,v, normed sid v))) cultures
    where
        bgs sid = backgroundFromBlanks (blankValsOf sid $ blanks) :: Background
        thds sid = (thresholdFromBlanks (blankValsOf sid $ blanks)) :: DetectionThreshold
        normed sid = normalize (bgs sid) (thds sid)


getAbsorbance = map (\(sid,mes) -> (sid, (\(AbsorbanceMeasurement x) -> x) . head $ filter filterAbs mes))
getFluorescence = map (\(sid,mes) -> (sid, (\(FluorescenseMeasurement x) -> x) . head $ filter filterFls mes))
getFluorescence2 = map (\(sid,mes) -> (sid, (\(FluorescenseMeasurement x) -> x) . head . tail $ filter filterFls mes))

filterFls (FluorescenseMeasurement _) = True
filterFls _ = False

filterAbs (AbsorbanceMeasurement _) = True
filterAbs _ = False

blanksOf :: SampleId -> [(SampleId,a)] -> [(SampleId,a)]
blanksOf sid = filter (\(sid2,_) -> sidExpId sid == sidExpId sid2 && sidPlate sid == sidPlate sid2)

blankValsOf :: (ColonySample a) => SampleId -> [(SampleId,a RawMeasurement)] -> [a RawMeasurement]
blankValsOf sid = map snd . blanksOf sid
