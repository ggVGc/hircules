module Lib
    ( Bot (..),
      Net,
      commands,
      hasURLs,
      lookupURLTitles,
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
import System.Exit
import System.IO
import System.Time
import Text.HTML.Scalpel
import Text.Printf

data Bot = Bot { socket :: Handle, channell :: String, starttime :: ClockTime }
type Net = ReaderT Bot IO

commands =
  [  -- ("quit"   , ("Quits the server"                 , handleQuit)), 
     ("help"    , ("Print out the help message"                    , handleHelp))
   , ("echo"    , ("Echo back the same string"                     , handleEcho))
   , ("join"    , ("Join a channel/channels. Eg. !join #foo,#bar"  , handleJoin))
   , ("uptime"  , ("Show the running time of the bot"              , handleUptime))
  ]

handleQuit n c l    =  write "QUIT" ":Exiting" >> liftIO exitSuccess

handleUptime n c l  =  uptime >>= privmsg n c

handleEcho          =  privmsg

handleHelp n c l
  | not (null . drop 1 $ splitOn " " l)
  = case _c of
    Just (message, _) -> privmsg n c $ command ++ " - " ++ message
    Nothing           -> privmsg n c "Command not found"
  where
    _c = lookup command commands
    command = head (splitOn " " l)

handleHelp n c l
  | null . drop 1 $ splitOn " " l
  = mapM_ printhelp commands
  where
    printhelp (command, (help, _)) = privmsg n c $ command ++ " - " ++ help

handleJoin n c l
  = case splitOn " " l of
    [chans] -> write "JOIN" chans
    _ -> privmsg n c l
  where
    printhelp c = case lookup c commands of
                  Just (help, _) -> privmsg n c $ c ++ " - " ++ help
                  Nothing -> return ()

privmsg :: String -> String -> String -> Net ()
privmsg _ chan s
  | "#" `isPrefixOf` chan
  = write "PRIVMSG" (chan ++ " :" ++ s)
privmsg nick _ s
  = write "PRIVMSG" (takeWhile (/= '!') nick ++ " :" ++ s)

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

lookupURLTitles :: String -> String -> String -> Net ()
lookupURLTitles nick chan s = do
  titles <- liftIO $ mapM scrapeTitle urls
  mapM_ (privmsg nick chan . unwords . lines) $ catMaybes titles
 where
    urls = filter isURL words
    words = splitOn " " s
    isURL s = "http://" `isPrefixOf` s ||
              "https://" `isPrefixOf` s

scrapeTitle :: String -> IO (Maybe String)
scrapeTitle u = scrapeURL u (text "title")
