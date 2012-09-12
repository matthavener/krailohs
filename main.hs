{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, NoMonoLocalBinds #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Map hiding (filter, map)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as U
import Text.CSV
import Data.Either
import Data.List as List
import Debug.Trace
import Text.Regex.PCRE 
import qualified Data.Map as Map
import System.Time

csvUrl = "http://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=csv"

data Opinion = Opinion
 {
     keyword :: String
   , response :: String
 } deriving (Show, Ord, Eq)

type Opinions = [Opinion]
type LastFireds = Map Opinion ClockTime

getOpinions :: IO (Opinions)
getOpinions = do
      rsp <- simpleHttp csvUrl
      let csv = U.decode $ L.unpack rsp
      -- just explode on error here
      let Right decoded = parseCSV "/" csv
      return [ Opinion { keyword = x , response = intercalate ", " xs } | (x:xs) <- decoded ]

regexWrapper x = ("(\\(|\\s|^)(" ++ x ++ ")(\\s|\\)|\\.|\\?|\\!|$)")
regexMatch regex str = 
  match ( makeRegexOpts compCaseless defaultExecOpt (regexWrapper regex) :: Regex) str

makeResponses :: Opinions -> String -> [(String, Opinion)]
makeResponses opinions msg | trace ("makeResponses " ++ show msg) False = undefined
makeResponses opinions msg = 
  [ (s ++ "? " ++ response o, o) | 
    o <- opinions, 
    let m = regexMatch (keyword o) msg, 
    let (_, _, _, (_:s:_)) = m :: (String, String, String, [String]), 
    m :: Bool ]

coolDown = noTimeDiff { tdHour = 2 }
removeEarly :: ClockTime -> LastFireds -> Opinions -> Opinions
removeEarly now lastFireds opinions | trace ("removeEarly " ++ show now ++ " " ++ show lastFireds ++ " " ++ show opinions) False = undefined
removeEarly now lastFireds opinions = 
    [ o | o <- opinions, 
      case Map.lookup o lastFireds of
        Just t -> ( addToClockTime coolDown t ) < now
        Nothing -> True ]
  
onMessage :: (TVar Opinions) -> (TVar LastFireds) -> EventFunc
onMessage opinionsTVar lastFiredsTVar s m | trace ("onMessage " ++ show m) False = undefined
onMessage opinionsTVar lastFiredsTVar s m
  | msg == "I CALL UPON THE POWER OF THE SPREADSHEET" = do
    sendMsg s chan "loading hacking tools..."
    -- todo spin off thread , delete old opinions from lastfireds
    opinions <- getOpinions
    atomically $ writeTVar opinionsTVar opinions
    sendMsg s chan "loaded tools"
  | otherwise = do
      now <- getClockTime
      (lastFireds, opinions) <- atomically $ do { lf <- readTVar lastFiredsTVar; o <- readTVar opinionsTVar; return (lf, o); }
      let opinions' = removeEarly now lastFireds opinions
      let responses = makeResponses opinions' msg
      sequence_ [sendMsg s chan $ B.pack r | (r, _) <- responses]
      let lastFireds' = List.foldl (\lf o -> Map.insert o now lf) lastFireds $ List.map snd responses
      atomically $ writeTVar lastFiredsTVar lastFireds'
  where chan = fromJust $ mChan m
        msg = B.unpack $ mMsg m

main = do
  opinions <- getOpinions
  opinionsTVar <- newTVarIO opinions
  lastFiredsTVar <- newTVarIO Map.empty 

  let events = [(Privmsg (onMessage opinionsTVar lastFiredsTVar))]

  let network = defaultConfig { 
      cAddr     = "irc.madhax.net" -- Address
    , cUsername = "umadbro"
    , cNick     = "krailohs" -- Nickname
    , cChannels = ["#main"] -- Channels to join on connect
    , cEvents   = events -- Events to bind
    }

  connect network True True
