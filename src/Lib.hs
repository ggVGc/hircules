module Lib
    ( Bot (..),
      Net,
      lookupURLTitles,
      hasURLs,
      privmsg,
      uptime,
      write
    ) where

import Control.Arrow
import Control.Exception
import Control.Monad (forever)
import Control.Monad.Reader
import Data.List
import Data.List.Split
import Data.Maybe
import Network
import System.IO
import System.Time
import Text.HTML.Scalpel
import Text.Printf

data Bot = Bot { socket :: Handle, channell :: String, starttime :: ClockTime }
type Net = ReaderT Bot IO

privmsg :: String -> Net ()
privmsg s = do
  chan<- asks channell
  write "PRIVMSG" (chan++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  liftIO $ hPrintf h "%s %s\r\n" s t
  liftIO $ printf "%s %s\r\n" s t

uptime :: Net String
uptime = do
  now <- liftIO getClockTime
  zero <- asks starttime
  return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td =
  unwords $ map (uncurry (++) . first show) $ 
  if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter ((/= 0) . fst) $ reverse $ snd $
                  foldl' merge (tdSec td,[]) metrics

hasURLs :: String -> Bool
hasURLs s = "http://" `isInfixOf` s || "https://" `isInfixOf` s 

lookupURLTitles :: String -> Net ()
lookupURLTitles s = do
  titles <- liftIO $ mapM scrapeTitle urls
  mapM_ privmsg $ catMaybes titles
 where
    urls = filter isURL words
    words = splitOn " " s
    isURL s = "http://" `isPrefixOf` s ||
              "https://" `isPrefixOf` s

scrapeTitle :: String -> IO (Maybe String)
scrapeTitle u = scrapeURL u (text "title")
