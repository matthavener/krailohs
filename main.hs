{-# LANGUAGE OverloadedStrings #-}
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
import Data.List
import Debug.Trace
import Text.Regex.TDFA 

csvUrl = "http://docs.google.com/spreadsheet/pub?hl=en_US&hl=en_US&key=0AoVJwKRm4drQdDdBY2hQWVBiSGtrMWsycGZzM0hKM3c&single=true&gid=0&output=csv"

data Opinion = Opinion
 {
     keyword :: String
   , response :: String
 } deriving (Show)

type Opinions = [Opinion]

getOpinions :: IO (Opinions)
getOpinions = do
      rsp <- simpleHttp csvUrl
      let csv = U.decode $ L.unpack rsp
      -- just explode on error here
      let Right decoded = parseCSV "/" csv
      return [ Opinion { keyword = x , response = intercalate ", " xs } | (x:xs) <- decoded ]

regexMatch :: String -> String -> Bool
regexMatch regex str = 
  match (makeRegexOpts (defaultCompOpt { caseSensitive = False } ) defaultExecOpt regex :: Regex) str

makeResponses :: Opinions -> String -> [String]
makeResponses opinions msg | trace ("makeResponses " ++ show msg) False = undefined
makeResponses opinions msg = 
  [ keyword o ++ "? " ++ response o | o <- opinions, regexMatch (keyword o) msg]
  
onMessage :: TVar Opinions -> EventFunc
onMessage opinionsTVar s m | trace ("onMessage " ++ show m) False = undefined
onMessage opinionsTVar s m
  | msg == "I CALL UPON THE POWER OF THE SPREADSHEET" = do
    sendMsg s chan "loading hacking tools..."
    -- todo spin off thread 
    opinions <- getOpinions
    atomically $ writeTVar opinionsTVar opinions
    sendMsg s chan "loaded tools"
  | otherwise = do
      opinions <- atomically $ readTVar opinionsTVar
      let responses = makeResponses opinions msg
      sequence_ [sendMsg s chan $ B.pack r | r <- responses]
  where chan = fromJust $ mChan m
        msg = B.unpack $ mMsg m

main = do
  opinions <- getOpinions
  opinionsTVar <- newTVarIO opinions

  let events = [(Privmsg (onMessage opinionsTVar))]

  let network = defaultConfig { 
      cAddr     = "irc.madhax.net" -- Address
    , cUsername = "umadbro"
    , cNick     = "krailohs" -- Nickname
    , cChannels = ["#test"] -- Channels to join on connect
    , cEvents   = events -- Events to bind
    }

  connect network False True
