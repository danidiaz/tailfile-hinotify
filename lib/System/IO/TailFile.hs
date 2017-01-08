{-# language NumDecimals #-}
module System.IO.TailFile (tailFile) where

import Data.Foldable
import qualified Data.ByteString
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import System.INotify
import System.IO (withFile,IOMode(ReadMode),hSeek,SeekMode(AbsoluteSeek,SeekFromEnd),hFileSize)
import System.IO.Error (isDoesNotExistError)

tailFile :: FilePath -> (a -> Data.ByteString.ByteString -> IO a) -> IO a -> IO void
tailFile filepath callback initial = withINotify $ \i -> do
    sem <- newEmptyMVar
    state <- initial
    _ <- addWatch i [Modify,MoveSelf,DeleteSelf] filepath (\event -> do _ <- tryTakeMVar sem
                                                                        putMVar sem event)
    loop sem state
    where
    loop sem =
        let go pristine a = do ea' <- tryJust (guard . isDoesNotExistError)
                                              (withFile filepath ReadMode (\h -> do if pristine then hSeek h SeekFromEnd 0
                                                                                                else return ()
                                                                                    sleeper sem h a))
                               case ea' of 
                                  Left ()  -> do threadDelay 5e5
                                                 go False a -- reuse the state
                                  Right a' -> go False a'
        in  go True
    sleeper sem h =
        let go ms a = do event <- takeMVar sem
                         size' <- hFileSize h 
                         for_ ms (\size -> if size' < size -- truncation detected
                                           then hSeek h AbsoluteSeek 0
                                           else return ())
                         a' <- drainBytes h a
                         case event of
                            MovedSelf {} -> return a'
                            Deleted {} -> return a'
                            _ -> do go (Just size') a'
        in  go Nothing
    drainBytes h = 
        let go a = do c <- Data.ByteString.hGetSome h defaultChunkSize
                      if Data.ByteString.null c
                         then do return a
                         else do a' <- callback a c
                                 drainBytes h a'
        in  go
