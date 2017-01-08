{-# language NumDecimals #-}
module System.IO.TailFile (tailFile) where

import qualified Data.ByteString
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import System.INotify
import System.IO (withFile,IOMode(ReadMode))
import System.IO.Error (isDoesNotExistError)

tailFile :: FilePath -> (a -> Data.ByteString.ByteString -> IO a) -> IO a -> IO void
tailFile filepath callback initial = withINotify $ \i -> do
    sem <- newEmptyMVar
    _ <- addWatch i [Modify,MoveSelf,DeleteSelf] filepath (\event -> do _ <- tryTakeMVar sem
                                                                        putMVar sem event)
    state <- initial
    loop sem state
    where
    loop sem =
        let go a = do ea' <- tryJust (guard . isDoesNotExistError)
                                     (withFile filepath ReadMode (\h -> sleeper sem h a))
                      case ea' of 
                         Left ()  -> do threadDelay 5e5
                                        go a -- reuse the state
                         Right a' -> go a'
        in  go
    sleeper sem h =
        let go a = do event <- takeMVar sem
                      a' <- drainBytes h a
                      case event of
                         MovedSelf {} -> return a'
                         Deleted {} -> return a'
                         _ -> go a'
        in  go
    drainBytes h = 
        let go a = do c <- Data.ByteString.hGetSome h defaultChunkSize
                      if Data.ByteString.null c
                         then do return a
                         else do a' <- callback a c
                                 drainBytes h a'
        in  go
