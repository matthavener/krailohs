{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Map hiding (filter, map)
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B

data Opinion = Opinion
 {
     keyword :: String
   , response :: String
 } deriving (Show)

type Opinions = [Opinion]

makeResponses :: Opinions -> String -> [String]
makeResponses opinions msg = 
  [ keyword o ++ "? " ++ response o | o <- opinions, msg == (keyword o) ]
  
onMessage :: MVar Opinions -> EventFunc
onMessage opinionsMVar s m
  | msg == "I CALL UPON THE POWER OF THE SPREADSHEET" = do
    sendMsg s chan "loading hacking tools..."
  | otherwise = do
      opinions <- takeMVar opinionsMVar
      let responses = makeResponses opinions msg
      sequence_ [sendMsg s chan $ B.pack r | r <- responses]
  where chan = fromJust $ mChan m
        msg = B.unpack $ mMsg m
        
main = do
  opinionsMVar <- newEmptyMVar
  _ <- putMVar opinionsMVar [ Opinion { keyword = "php" , response = "sucks" } ]

  let events = [(Privmsg (onMessage opinionsMVar))]

  let network = defaultConfig { 
      cAddr     = "irc.madhax.net" -- Address
    , cNick     = "krailohs" -- Nickname
    , cChannels = ["#main"] -- Channels to join on connect
    , cEvents   = events -- Events to bind
    }

  connect network False True
