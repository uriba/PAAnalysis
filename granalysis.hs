import Biolab.Interfaces.MySql
import Biolab.Analysis.GrowthRate
import Biolab.Analysis.Normalization
import Biolab.Types
import Biolab.Analysis.Types
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Time (UTCTime, NominalDiffTime)

main = do
    db_conf <- dbConnectInfo "db.conf"
    let row = 'c'
    let blank_wells = [Well 'e' 1, Well 'e' 12]
    let wells =  blank_wells ++ map (Well row) [1..12]
    let sq = SampleQuery ["2012-11-20 12:00:00"] [0..5] wells
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
    let res = zipWith3 (\a b c -> if (fst a == fst b && fst a == fst c) then (fst a,(snd a, snd b, snd c)) else error $ "colony id mismatch:" ++ show (fst a) ++ show (fst b) ++ show (fst c)) rgods rgfls rgfl2s
    mapM (putStrLn . show) res
    return ()

getGrs :: (ColonySample a) => [(SampleId, a RawMeasurement)] -> [(SampleId, a RawMeasurement)] -> [(SampleId, Maybe NominalDiffTime)]
getGrs blanks cultures = map (\(sid,v) -> (sid, minDoublingTime . normalize (bgs sid) (thds sid) $ v)) cultures
    where
        bgs sid = backgroundFromBlanks (blankValsOf sid $ blanks) :: Background
        thds sid = (thresholdFromBlanks (blankValsOf sid $ blanks)) :: DetectionThreshold


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
