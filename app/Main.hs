module Main where

import Data.List
import Data.List.Split
import Control.Arrow
import Control.Exception
import Control.Monad (forever)
import Control.Monad.Reader
import Network
import System.Exit
import System.IO
import System.Time
import Text.Printf

import Lib

server = "irc.freenode.org"
port = 6667
chan = "#hircules"
nick = "hircules"
commandChar = '!'

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop = runReaderT run

connect :: IO Bot
connect = notify $ do
  t <- getClockTime
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return (Bot h chan t)
 where
  notify = bracket_
      (printf "Connecting to %s ... " server >> hFlush stdout)
      (putStrLn "done.")

run :: Net ()
run = do
  write "NICK" nick
  write "USER" (nick++" 0 * :hircules bot")
  write "JOIN" chan
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` liftIO (hGetLine h)
  liftIO (putStrLn s)
  if ping s 
  then pong s 
  else when (isprivmsg s) $
            let (n, c, l) = splitprivmsg s
            in eval n c l
 where
    clean = drop 1 . dropWhile (/= ':') . drop 1
    isprivmsg = isPrefixOf "PRIVMSG" . drop 1 . dropWhile (/= ' ') . drop 1 
    splitprivmsg s =
      (n, c, line)
      where
        [n, _, c] = splitOn " " $ drop 1 $ takeWhile (/= ':') $ drop 1 s
        line = clean s
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

-- :AshyIsMe!~aaron@unaffiliated/ashyisme PRIVMSG #hircules :yo
-- :AshyIsMe!~aaron@unaffiliated/ashyisme PRIVMSG hircules :yo

isCommand :: String -> Bool
isCommand = not . isPrefixOf [commandChar]

commands =
  [  ("quit"   , ("Quits the server"                 , handleQuit))
   , ("uptime" , ("Show the running time of the bot" , handleUptime))
   , ("id"     , ("Echo back the same string"        , handleId))
  ]

handleQuit n c l = write "QUIT" ":Exiting" >> liftIO exitSuccess
handleUptime n c l = uptime >>= privmsg
handleId n c l = privmsg l

eval :: String -> String -> String -> Net ()
eval nick chan line
  | [commandChar] `isPrefixOf` line
  = case lookup command commands of
      Just (docs, f) -> f nick chan args
      Nothing -> privmsg "Command not found."
  where
    command = takeWhile (/= ' ') $ drop 1 line
    args = drop 1 $ dropWhile (/= ' ') line

eval nick chan line
  | hasURLs line
  = lookupURLTitles line

eval _ _ _ = return ()
