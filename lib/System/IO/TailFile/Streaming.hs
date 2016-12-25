{-# language RankNTypes #-}
module System.IO.TailFile.Streaming where

import qualified Data.ByteString
import Streaming
import Streaming.Eversion
import qualified System.IO.TailFile.Foldl

tailFile :: FilePath -- ^ 
         -> (forall t r. (MonadTrans t, MonadIO (t IO)) => Stream (Of Data.ByteString.ByteString) (t IO) r 
                                                        -> t IO (Of void r)) -- ^ fold
         -> IO void
tailFile path consumer = System.IO.TailFile.Foldl.tailFile path (evertMIO consumer) 
