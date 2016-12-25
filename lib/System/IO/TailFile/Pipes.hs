{-# language RankNTypes #-}
module System.IO.TailFile.Pipes where

import qualified Data.ByteString
import Pipes
import Streaming.Eversion.Pipes
import qualified System.IO.TailFile.Foldl

tailFile :: FilePath 
         -> (forall t r. (MonadTrans t, MonadIO (t IO)) => Producer Data.ByteString.ByteString (t IO) r -> t IO (void, r))
         -> IO void
tailFile path consumer = System.IO.TailFile.Foldl.tailFile path (evertMIO consumer) 
