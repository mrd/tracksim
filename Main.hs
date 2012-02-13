import TrackSim
import Text.Printf
import Control.Monad
import Data.List
import System.Environment
import System.Console.GetOpt

data Options =
  Options { optMaxSpd
          , optAccel
          , optDecel
          , optLength
          , optUnit    :: Double
          , optTrkFile :: Maybe FilePath
          , optCarName :: String
          , optCount   :: Int
          , optQuiet   :: Bool }
  deriving Show

defaultOptions =
  Options { optMaxSpd   = mph2mps 30
          , optAccel    = mph2mps 2.8
          , optDecel    = mph2mps (-3.5)
          , optLength   = 22
          , optUnit     = 100
          , optTrkFile  = Nothing
          , optCarName  = "Type 7"
          , optCount    = 1
          , optQuiet    = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['S'] ["maxspeed"]
    (ReqArg (\ d opts -> opts { optMaxSpd = read d }) "mps") "max speed limit"
  , Option ['A'] ["accel"]
    (ReqArg (\ d opts -> opts { optAccel = read d }) "mps^2") "acceleration"
  , Option ['D'] ["decel"]
    (ReqArg (\ d opts -> opts { optDecel = read d }) "mps^2") "deceleration"
  , Option ['U'] ["unit"]
    (ReqArg (\ d opts -> opts { optUnit = read d }) "ticks") "unit ticks per second"
  , Option ['L'] ["length"]
    (ReqArg (\ d opts -> opts { optLength = read d }) "m") "vehicle length"
  , Option ['N'] ["name"]
    (ReqArg (\ d opts -> opts { optCarName = d }) "name") "vehicle name"
  , Option ['n'] ["count"]
    (ReqArg (\ d opts -> opts { optCount = read d }) "number") "number of times to run"
  , Option ['q'] ["quiet"]
    (NoArg (\ opts -> opts { optQuiet = True })) "quiet mode" ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,trkFile:n,[]  ) -> return ((foldl (flip id) defaultOptions o) { optTrkFile = Just trkFile }, n)
    (_,_,errs)         -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: [OPTION...] file"

main = do
  (opts, _) <- parseOpts =<< getArgs
  let Just f = optTrkFile opts
  let unit = optUnit opts
  let car = Car (optCarName opts) (optMaxSpd opts) (optAccel opts) (optDecel opts) (optLength opts)
  st <- replicateM (optCount opts) ((flip runSim step) `fmap` mkSimState unit car f)

  case optCount opts of
    1 -> forM_ (sort (simLog (head st))) $ \ e ->
            putStrLn (timeString (simEvtTime e / unit) ++ ": " ++ simEvtName e ++ ": " ++
                case simEvtTP e of
                  CR {} -> tpName (simEvtTP e) ++ " crossing"
                  ST {} -> tpName (simEvtTP e) ++ " station"
                  LI li -> show li)
    n -> do ts <- forM (map (last . sort . simLog) st) $ \ e -> do
                    unless (optQuiet opts) $
                      printf "%f (%s)\n" (simEvtTime e / unit) (timeString (simEvtTime e / unit))
                    return (simEvtTime e / unit)
            let avg = (sum ts / fromIntegral (length ts))
            printf "Average: %f (%s)\n" avg (timeString avg)

mkSimState u c f = do
  e_tr <- readTrackFile f
  case e_tr of
    Left err -> fail (show err)
    Right tr -> mkStateIO u (trackCar c) tr
