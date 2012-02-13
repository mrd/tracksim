import TrackSim
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
          , optCarName :: String }
  deriving Show

defaultOptions =
  Options { optMaxSpd   = mph2mps 30
          , optAccel    = mph2mps 2.8
          , optDecel    = mph2mps (-3.5)
          , optLength   = 22
          , optUnit     = 100
          , optTrkFile  = Nothing
          , optCarName  = "Type 7" }

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
    (ReqArg (\ d opts -> opts { optCarName = d }) "name") "vehicle name" ]

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,trkFile:n,[]  ) -> return ((foldl (flip id) defaultOptions o) { optTrkFile = Just trkFile }, n)
    (_,_,errs)         -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: [OPTION...] file"

main = do
  (opts, _) <- parseOpts =<< getArgs
  let Just f = optTrkFile opts
  let car = Car (optCarName opts) (optMaxSpd opts) (optAccel opts) (optDecel opts) (optLength opts)
  st <- (flip runSim step) `fmap` mkSimState (optUnit opts) car f
  let unit = simTickUnit st
  forM_ (sort (simLog st)) $ \ e ->
    putStrLn (timeString (simEvtTime e / unit) ++ ": " ++ simEvtName e ++ ": " ++
        case simEvtTP e of
          CR {} -> tpName (simEvtTP e) ++ " crossing"
          ST {} -> tpName (simEvtTP e) ++ " station"
          LI li -> show li
      )

mkSimState u c f = do
  e_tr <- readTrackFile f
  case e_tr of
    Left err -> fail (show err)
    Right tr -> mkStateIO u (trackCar c) tr
