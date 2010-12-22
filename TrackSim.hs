module TrackSim where
import Parser
import TravelTime
import Data.List
import System.Random
import Control.Monad
import Control.Monad.State

-- Unless otherwise mentioned, time is in seconds, distance is in
-- meters, speed is in meters per second, and acceleration is in
-- meters per second squared.

mi2km = (* 1.609344)
kph2mps = (* 1000) . (/ 3600)
mph2mps = kph2mps . mi2km

-- | Track Point
data TP = -- | Unspecified Track Point (with index)
          TP { tpName :: String, tpNum :: Int }
          -- | Station with name, average delay, and standard deviation
        | ST { tpName :: String, stDelayAvg, stDelayStdDev :: Double }
          -- | Crossing (or signal) with name, cycle duration, "green"
          -- duration, and whether or not to always stop regardless
          -- of green (e.g. Stop Sign).
        | CR { tpName :: String, crCycle, crGreen :: Double, crAlwaysStop :: Bool }
          -- | New Speed Limit.
        | LI { liMax :: Double }
  deriving Show
-- | A Track is a series of Track Points associated with distance along the track.
type Track = [(TP, Double)]

-- | A car has a name, maximum speed, acceleration, braking and length characteristics.
data Car = Car { carName :: String, carMaxSpeed, carAccel, carBraking, carLength :: Double }
  deriving Show
-- | A car on a track has a speed limit.
data CarTrack = CT { ctCar :: Car, ctSpeedLimit :: Double }
  deriving Show

instance Vehicle Car where
  vehicleAccel = carAccel; vehicleBrake = carBraking; vehicleMaxSpeed = carMaxSpeed
instance Vehicle CarTrack where
  vehicleAccel    = vehicleAccel . ctCar
  vehicleBrake    = vehicleBrake . ctCar
  vehicleMaxSpeed = ctSpeedLimit

-- | Associate a car with a track.
trackCar c = CT { ctCar = c, ctSpeedLimit = carMaxSpeed c }

-- | Compute average waiting time at a crossing.
avgCRdelay (CR _ cyc grn _) = (cyc - grn)**2 / (2 * cyc)
avgCRdelay _                = 0

-- | bigStep advances time by computing how long it takes to travel
-- between each track point and moving directly there.  It uses some
-- simple approximations to compute delay time at each crossing or
-- station.
bigStep (ct, x, t, [])     = (ct, x, t, [])
bigStep (ct, x, t, (tr, nextX):trs) = (ct', x', t'', trs)
  where
    t' = t + timeToTravel ct' (nextX - x); x' = nextX
    t'' = t' + (case tr of ST { stDelayAvg = avg } -> avg; _ -> avgCRdelay tr)
    ct' = case tr of LI max -> ct { ctSpeedLimit = max }; _ -> ct

-- | bigStepSim performs its simulation by big-stepping from track
-- point to track point.
bigStepSim c tr = steps1 ++ [head steps2]
  where
    steps            = iterate bigStep (trackCar c, 0, 0, tr)
    (steps1, steps2) = break (\ (_, _, _, tr) -> null tr) steps

-- | Show an approximate time duration in human-friendly format.
timeString t = show min ++ " minute" ++ pluralmin ++ " " ++ show sec ++ " second" ++ pluralsec
  where
    min       = round t `div` 60
    pluralmin = if min == 1 then "" else "s"
    sec       = round t `rem` 60
    pluralsec = if sec == 1 then "" else "s"

stDefaultAvg = 30
stDefaultStdDev = 5
crDefaultCyc = 90
crDefaultGrn = 30

-- | Parse and interpret the values in a track file, producing a Track upon success.
readTrackFile f = do
  e_es <- parseTrackFile f
  case e_es of
    Left err -> return $ Left err
    Right es -> return . Right . concat . snd . mapAccumL doEntry 0 $ es
  where
    doEntry x (Comment _) = (x, [])
    doEntry x (Entry (d, t, a, n)) =
      case (t, a) of
        ("LI", li:_)          -> (d+x, [(LI li, d+x)])
        ("ST", [])            -> (d+x, [(ST n stDefaultAvg stDefaultStdDev, d+x)])
        ("ST", avg:std:_)     -> (d+x, [(ST n avg std, d+x)])
        ("CR", [])            -> (d+x, [(CR n crDefaultCyc crDefaultGrn False, d+x)])
        ("CR", cyc:grn:stp:_) -> (d+x, [(CR n cyc grn (stp == 1), d+x)])
        ("TP", _)             -> (d+x, [(TP n 0, d+x)])
        _                     -> (d+x, [])

bigStepSimTrackFile c f = do
  e_tr <- readTrackFile f
  case e_tr of
    Left err -> return $ Left err
    Right tr -> return $ Right (bigStepSim c tr)

testTrackFile c f = do
  e_sim <- bigStepSimTrackFile c f
  case e_sim of
    Left err -> print err
    Right sim -> mapM_ (\ (_, x, t, _) -> putStrLn (show (round x) ++ ": " ++ timeString t)) sim

--------------------------------------------------

-- | SimEvents are used to record interesting points of time in the log.
data SimEvent = Evt { simEvtTime :: Double, simEvtName :: String, simEvtTP :: TP }
  deriving Show
instance Eq SimEvent where
  Evt { simEvtTime = t1, simEvtName = n1 } == Evt { simEvtTime = t2, simEvtName = n2 }
    = (t1, n1) == (t2, n2)
instance Ord SimEvent where
  Evt { simEvtTime = t1 } `compare` Evt { simEvtTime = t2 } = compare t1 t2

-- | Current state of the simulation for one vehicle
data SimState =
  St {
     -- |current position
       simPosition
     -- |current speed
     , simSpeed
     -- |current acceleration
     , simAccel
     -- |if non-zero, then vehicle is waiting (in ticks)
     , simWaitTicks
     -- |current time tick
     , simTick
     -- |number of ticks per second
     , simTickUnit :: Double
     -- |list of upcoming acceleration changes (tick, accel)
     , simDeltas   :: [(Double, Double)]
     -- |vehicle being simulated
     , simVehicle  :: CarTrack
     -- |track being simulated
     , simTrack    :: Track
     -- |random number generator
     , simRNG      :: StdGen
     -- |log of important events
     , simLog      :: [SimEvent]
  } deriving Show

-- | Simulation Monad
type SimM a = State SimState a

-- "Approximate" version of comparison functions for working with
-- inexact values represented as Doubles.
approxLT e x y = x + e < y
approxGT e x y = x - e > y
approxEQ e x y = not (approxLT e x y) && not (approxGT e x y)

-- | Precision of approximation is determined by the time unit, in
-- this case I have chosen to make it the reciprocal of unit.
unitEpsilon unit = 1 / unit

-- | Uses "simple" approximation by adding 12 random variables [0,1]
-- and subtracting 6 to produce a N(0,1) random variable X.
normalX :: RandomGen g => Double -> Double -> g -> (Double, g)
normalX avg stddev rng = (x, snd (last ls))
  where
    ls = take 12 . drop 1 $ iterate (\ (_, g) -> randomR (0, 1) g) (0, rng)
    x  = stddev * (sum (map fst ls) - 6) + avg

-- | Figure out waiting time (in seconds) until safe to cross.  For now
-- assume the "green" portion of the cycle comes at the beginning.
-- Expects t in seconds.
nextCrossing t' (CR _ cyc' grn' stop)
  | t `mod` cyc < grn = if stop then 1 else 0
  | otherwise         = fromIntegral $ cyc - (t `mod` cyc)
  where t = round t'; cyc = round cyc'; grn = round grn'
nextCrossing _ _ = 0

-- | "Little" step simulates one tick at a time, moving the vehicle a
-- small ways down the track and handling each track point only when it is reached.
step :: SimM ()
step = do
  st@St { simPosition = x, simSpeed = v, simAccel = a, simTick = t } <- get
  let (deltas, deltas') = span ((<= t) . fst) (simDeltas st)
  let veh = simVehicle st; unit = simTickUnit st
  let epsilon = unitEpsilon unit
  let lt = approxLT epsilon; eq = approxEQ epsilon
  -- update physics variables
  let a' = if null deltas then a else snd (last deltas)
  let v' = max 0 (v + a' / unit)
  let x' = x + v' / unit
  put $ st { simPosition = x', simSpeed = v', simAccel = a'
           , simDeltas = deltas' }
  st <- get
  -- decide what to do based on track location
  st' <- case simTrack st of
    -- if simWaitTicks > 0 then we don't do anything
    _ | simWaitTicks st > 0 -> return st
    -- else if we have not reached the next track point then compute travel
    (_, xt):_ | x' `lt` xt -> do
      -- check if velocity is approximately zero, and there are no scheduled accelerations
      if v' `eq` 0 && null deltas'
        then do
          -- FIXME: not all TPs require a stop, but this does not account for that
          let (tA,tB,tC) = timeTriple veh (xt - x')
          -- tA=accel time; tB=brake time; tC=cruise time
          -- update deltas accordingly
          return $ st { simDeltas = [ (t + 1, vehicleAccel veh)
                                    , (t + 1 + flr (tA * unit), 0)
                                    , (t + 1 + flr ((tA + tC) * unit), vehicleBrake veh) ] }
         -- else, just coast according to schedule
         else return st
    -- else we have reached a track point, do something
    (tr, xt):trs' -> do
      st1 <- case tr of
        -- Speed Limit change
        LI max -> do
          let log' = Evt t "New Speed Limit" tr : simLog st
          return $ st { simVehicle = veh { ctSpeedLimit = max }, simLog = log' }
        -- Station stop
        ST name avg dev -> do
          -- choose a waiting time from a normal distribution
          let (delay', rng') = normalX avg dev (simRNG st)
          let delay = fromIntegral . round $ delay' * unit
          let log' = Evt t ("Arrival "++show delay) tr : Evt (t + delay) "Departure" tr : simLog st
          return $ st { simWaitTicks = delay, simLog = log', simRNG = rng' }
        -- Crossing
        CR name cyc grn stop -> do
          -- wait at crossing until next "green" part of cycle
          let delay = fromIntegral . round $ nextCrossing (t / unit) tr * unit
          let log' = Evt t ("Arrival "++show delay) tr : Evt (t + delay) "Departure" tr : simLog st
          return $ st { simWaitTicks = delay, simLog = log' }
        -- Other
        _ -> return st
      -- done with track point, cut it from the list
      return $ st1 { simTrack = trs' }
    -- all other cases: do nothing
    _                      -> return st
  put $ st' { simTick = simTick st' + 1
            , simWaitTicks = max 0 (simWaitTicks st' - 1) }

flr = fromIntegral . floor

-- | Create an initial state from random-number generator, number of
-- ticks per second, a vehicle, and a track.
mkState rng unit veh tr =
  St { simPosition   = 0
     , simSpeed      = 0
     , simAccel      = 0
     , simTick       = 0
     , simTickUnit   = unit
     , simWaitTicks  = 0
     , simDeltas     = []
     , simVehicle    = veh
     , simTrack      = tr
     , simRNG        = rng
     , simLog        = [] }

-- | Convenience function which gets the random-number generator from
-- the system.
mkStateIO unit veh tr = do
  rng <- newStdGen
  return $ mkState rng unit veh tr

-- | Runs the simulation until track is exhausted and produces the
-- final state.
runSim st0 f
  | null (simTrack st0) = st0
  | otherwise = runSim (execState f st0) f

--------------------------------------------------
-- Some testing code

testUnit = 100
mkTestState1 = mkStateIO testUnit (trackCar testCar) testTrack
mkTestState = do
  e_tr <- readTrackFile "B_Line_Outbound.txt"
  case e_tr of
    Left err -> fail (show err)
    Right tr -> mkStateIO testUnit (trackCar testCar) tr

test = do
  st <- (flip runSim step) `fmap` mkTestState
  let unit = simTickUnit st
  forM_ (sort (simLog st)) $ \ e ->
    putStrLn (timeString (simEvtTime e / unit) ++ ": " ++ simEvtName e ++ ": " ++
        case simEvtTP e of
          CR {} -> tpName (simEvtTP e) ++ " crossing"
          ST {} -> tpName (simEvtTP e) ++ " station"
          LI li -> show li
      )

testCar :: Car
testCar = Car "Type 7" (mph2mps 60) 2.68 (-0.981) 6

testTrack :: Track
testTrack = [(LI (mph2mps 25), 0), (TP "Start" 0, 0), (ST "St1" 30 5, 20), (CR "Cr1" 90 45 False, 21)]
