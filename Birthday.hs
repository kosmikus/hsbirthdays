--
-- hsbirthdays
-- (c) 2005-2025 Andres Loeh <hsbirthdays at andres-loeh dot de>
--

import System.Time
import System.Environment
import Control.Monad.State
import Data.List
import Data.Version
import System.Console.GetOpt

import Paths_hsbirthdays

progname :: String
progname = "hsbirthdays"

data Birthday =
  B
    Bool         -- ^ has year?
    CalendarTime -- ^ when?
    String       -- ^ descriptive text

type Errors = [String]

data Opts = O
  { files :: [String]
  , days  :: !Int
  , age   :: !Bool
  , years :: !Bool
  }

defaults :: IO Opts
defaults =  do
  home <- getEnv "HOME"
  pure O{ files = [home ++ "/." ++ progname], days = 28, age = True, years = True }

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option "f" ["file"]    (ReqArg (\ s o -> o{ files = s : files o }) "name") "specify birthdays file(s)"
  , Option "d" ["days"]    (ReqArg (\ d o -> o{ days = (read d) }) "number")   "how many days"
  , Option []  ["noage"]   (NoArg (\ o -> o{ age = False }))                   "don't show age"
  , Option []  ["noyears"] (NoArg (\ o -> o{ years = False }))                 "don't show year numbers"
  ]

main :: IO ()
main = do
  dopts <- defaults
  args <- getArgs
  let (opts,errs,nopts) = getOpt Permute options args
  if not (null (errs ++ nopts))
    then putStrLn $ usageInfo ("hsbirthdays, version " ++ showVersion version ++ "\nusage: " ++ progname ++ " [options]") options
    else proceed (foldl' (flip ($)) dopts opts)

proceed :: Opts -> IO ()
proceed opts = do
  (birthdays,errors) <- parseFiles $ files opts
  mapM_ putStrLn errors
  time <- getClockTime
  ctime <- toCalendarTime time
  let year = ctYear ctime
  -- real times
  let rt = concatMap (realTime year) birthdays
  -- select timespan of interest
  let sel = filter (\(x,_) ->  let diff  = normalizeTimeDiff $ diffClockTimes (toClockTime x) time
                                   rdiff = normalizeTimeDiff $ noTimeDiff { tdDay = days opts }
                                   ndiff = normalizeTimeDiff $ noTimeDiff { tdDay = -1 } -- we want todays
                               in  ndiff < diff && diff <= rdiff) rt
  mapM_ putStrLn $ map (uncurry (printBirthday opts)) (sortOn fst sel)

interestingLine :: String -> Bool
interestingLine ""       = False
interestingLine ('#': _) = False
interestingLine _        = True

parseFiles :: [FilePath] -> IO ([Birthday], Errors)
parseFiles f = do
  xs <- readFiles f
  let
    -- one line per birthday
    entries :: [String]
    entries = lines xs

    -- add line numbers
    nrentries :: [(Int, String)]
    nrentries = zip [1..] entries -- line numbers

    -- drop blank lines and comments
    stripped :: [(Int, String)]
    stripped = filter (interestingLine . snd) nrentries

    split :: [(Int, [String])]
    split = map (\ (x,y) -> (x,words y)) stripped

    (birthdayss,errors) = runState (mapM parseBirthday split) []
  pure (concat birthdayss, errors)

-- the last entry is the default; if user-specified files are present, ignore it
readFiles :: [FilePath] -> IO String
readFiles [f] = readFile f
readFiles fs  = mapM readFile (init fs) >>= pure . concat

addError :: String -> State Errors ()
addError n = modify (n:)

realTime :: Int -> Birthday -> [(CalendarTime,Birthday)]
realTime y b@(B _ t _) = [(t{ ctYear = y }, b), (t{ ctYear = y+1 }, b)]

printBirthday :: Opts -> CalendarTime -> Birthday -> String
printBirthday opts t (B b o txt) =
   let
     d = ctDay t
     m = fromEnum (ctMonth t) + 1
     y = ctYear t
     date =
       formatTwo d ++ "." ++ formatTwo m ++ "."
       ++ if years opts then show y else ""
   in
     date ++ " " ++ txt ++
     if b && age opts then " (" ++ show (y - ctYear o) ++ ")" else ""

formatTwo :: Int -> String
formatTwo n = (if n < 10 then "0" else "") ++ show n

parseBirthday :: (Int, [String]) -> State Errors [Birthday]
parseBirthday (n, d : txt@(_ : _)) = do
  x <- parseDate n d
  case x of
    Nothing      -> pure []
    Just (y, pd) -> pure [B y pd (unwords txt)]
parseBirthday (n, _)              = do
  addError $ show n ++ ": malformed line"
  pure []

parseDate :: Int -> String -> State Errors (Maybe (Bool,CalendarTime))
parseDate n date = do
  is <- parseDotSequence n date
  case is of
    [d, m, y] -> pure (Just (True,  emptyTime { ctDay    =  d,
                                                  ctMonth  =  toEnum (m-1),
                                                  ctYear   =  y
                                                }
                        ))
    [d, m]    -> pure (Just (False, emptyTime { ctDay    =  d,
                                                  ctMonth  =  toEnum (m-1)
                                                }
                        ))
    _        ->  do
      addError $ show n ++ ": malformed date"
      pure Nothing

emptyTime :: CalendarTime
emptyTime = toUTCTime (TOD 0 0)

parseDotSequence :: Int -> String -> State Errors [Int]
parseDotSequence n s =
   case findIndex (=='.') s of
     Nothing ->
       case s of
         [] -> pure []
         _  -> case reads s of
                 [(i,"")] -> pure [i]
                 _ -> do  addError $ show n ++ ": malformed date"
                          pure []
     Just k  ->
       case splitAt k s of
         (pre, _:post) -> case reads pre of
                            [(i,"")] -> do  rs <- parseDotSequence k post
                                            pure (i : rs)
                            _ -> do addError $ show k ++ ": malformed date"
                                    pure []
         (_, []) -> do addError $ show k ++ ": malformed date"
                       pure []
