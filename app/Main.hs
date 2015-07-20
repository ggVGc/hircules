{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where


import           Control.Arrow
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Data.List
import           Data.List.Split
import           Data.Yaml
import           Network
import           System.Directory
import           System.IO
import           System.Time
import           System.Posix.Files
import           Text.Printf
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Lib

{-TODO - skinflutery: s/foo/bar-}

fifoname = ".hircules-fifo"

main :: IO ()
main = do
  yamldata <- BS.readFile "config.yaml"
  case decode yamldata :: Maybe [HirculesConfig] of
    Just [conf] -> bracket (connect conf) disconnect loop
        where
          disconnect = hClose . socket
          loop = runReaderT (run conf)
    Just cs -> putStrLn $ "Error in config.yaml: " ++ show cs
    Nothing -> putStrLn "Error in config.yaml"

connect :: HirculesConfig -> IO Bot
connect conf = notify $ do
  t <- getClockTime
  h <- connectTo (T.unpack $ server conf) (PortNumber (fromIntegral (port conf)))
  hSetBuffering h NoBuffering
  return (Bot h (T.unpack $ chans conf) (commandChar conf) t)
 where
  notify = bracket_
      (printf "Connecting to %s ... " (T.unpack $ server conf) >> hFlush stdout)
      (putStrLn "done.")

run :: HirculesConfig -> Net ()
run conf = do
  write "NICK" (T.unpack $ nick conf)
  write "USER" (T.unpack (nick conf) ++" 0 * :hircules bot")
  write "JOIN" (T.unpack $ chans conf)
  asks socket >>= listen

listen :: Handle -> Net ()
listen h = do
  fileexists <- liftIO $ doesFileExist fifoname
  when fileexists (liftIO $ removeFile fifoname)
  fifo <- liftIO $ createNamedPipe fifoname accessModes
  let processIRC = forever $ do
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
              [n, _, c, _] = splitOn " " $ takeWhile (/= ':') $ drop 1 s
              line = clean s
          ping x = "PING :" `isPrefixOf` x
          pong x = write "PONG" (':' : drop 6 x)

      processFIFO = forever $ do
        chan <- asks channel
        s <- liftIO $ readFile fifoname
        mapM_ (privmsg "" chan) $ lines s
    in do 
      bot <- ask
      liftIO $ concurrently (runReaderT processIRC bot) (runReaderT processFIFO bot)
      return ()

-- :nickname!~user@unaffiliated/nickname PRIVMSG #hircules :yo
-- :nickname!~user@unaffiliated/nickname PRIVMSG hircules :yo

eval :: String -> String -> String -> Net ()
eval nick chan line = do
  _commandChar <- asks comChar
  case [_commandChar] `isPrefixOf` line of
    True -> case lookup command commands of
              Just (docs, f) -> f nick chan args
              Nothing -> privmsg nick chan "Command not found."
          where
            command = takeWhile (/= ' ') $ drop 1 line
            args = drop 1 $ dropWhile (/= ' ') line
    False -> when (hasURLs line) $
               lookupURLTitles nick chan line
