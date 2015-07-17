module Main where

import Data.List
import Control.Monad (forever)
import Network
import System.Exit
import System.IO
import Text.Printf

{-import Lib-}

server = "irc.freenode.org"
port = 6667
chan = "#hircules"
nick = "hircules"

main :: IO ()
{-main = someFunc-}

main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick++" 0 * :hircules bot")
  write h "JOIN" chan
  listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf "%s %s\r\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
  t <- hGetLine h
  let s = init t
  if ping s then pong s else eval h (clean s)
  putStrLn s
 where
    clean = drop 1 . dropWhile (/= ':') . drop 1

    ping x = "PING :" `isPrefixOf` x
    pong x = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval h "!quit"                   = write h "QUIT" ":Exiting" >> exitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _ _                         = return ()

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)
