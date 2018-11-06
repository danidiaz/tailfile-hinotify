{-| Tail files in Linux.

    The functions in this module do not use any particular streaming library.
    They just accept an initial state and a monadic update function.
 -}


{-# language NumDecimals #-}
{-# language BangPatterns #-}
module System.IO.TailFile (tailFile) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Foldable
import Data.Monoid
import qualified Data.Text
import qualified Data.Text.Encoding
import System.INotify
import System.IO (withFile
                 ,IOMode(ReadMode)
                 ,hSeek
                 ,SeekMode(AbsoluteSeek,SeekFromEnd)
                 ,hFileSize)
import System.IO.Error (isDoesNotExistError)

{-| Tail a file, while keeping an internal state.

    If the file doesn't exist, `tailFile` will poll for it until it is found.

    If `tailFile` detects the file has been moved or renamed, it goes back to
    watching a file with the original name.

    `tailFile` also detects file truncations, in which case it starts reading
    again from the beginning.

    Data already existing in the file before `tailFile` is invoked is ignored.
 -}
tailFile :: FilePath
         -> (a -> Data.ByteString.ByteString -> IO a) -- ^ State update function.
         -> IO a -- ^ Monadic action for getting the initial state.
         -> IO void -- ^ The result action never returns!
tailFile filepath callback initial = withINotify (\i ->
    do state <- initial
       loop i state)
    where
    loop i =
        let go pristine a = do ea' <- tryJust (guard . isDoesNotExistError)
                                              (watchFile pristine i a)
                               case ea' of
                                  Left ()  -> do threadDelay 5e5
                                                 go False a -- reuse the state
                                  Right a' -> go False a'
        in  go True
    watchFile pristine i a =
        do sem <- newMVar mempty
           bracket (addWatch i
                             [Modify,MoveSelf,DeleteSelf]
                             (Data.Text.Encoding.encodeUtf8 (Data.Text.pack filepath))
                             (\event -> let stop = Any (case event of
                                                           MovedSelf {} -> True
                                                           Deleted {} -> True
                                                           _ -> False)
                                        in do old <- fold <$> tryTakeMVar sem
                                              new <- evaluate $ old <> stop
                                              putMVar sem new))
                   removeWatch
                   (\_ -> withFile filepath ReadMode (\h ->
                              do when pristine
                                      (hSeek h SeekFromEnd 0)
                                 sleeper sem h a))
    sleeper sem h =
        let go ms a = do event <- takeMVar sem
                         size' <- hFileSize h
                         for_ ms (\size -> when (size' < size) -- truncation
                                                (hSeek h AbsoluteSeek 0))
                         !a' <- drainBytes h a
                         if getAny event then return a'
                                         else go (Just size') a'
        in  go Nothing
    drainBytes h =
        let go a = do c <- Data.ByteString.hGetSome h defaultChunkSize
                      if Data.ByteString.null c
                         then do return a
                         else do !a' <- callback a c
                                 go a'
        in  go
