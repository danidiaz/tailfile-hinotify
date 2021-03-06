{-| Tail files in Unix, using types from the @streaming@ package.

 -}
{-# LANGUAGE RankNTypes #-}
module System.IO.TailFile.Streaming where

import           Data.ByteString          (ByteString)
import           Streaming
import           Streaming.Eversion
import qualified System.IO.TailFile.Foldl

{-| Tail a file with a function that consumes a 'Stream'.
-}
tailFile :: FilePath
         -> (forall t r. (MonadTrans t, MonadIO (t IO)) => Stream (Of Data.ByteString.ByteString) (t IO) r -> t IO (Of void r)) -- ^ Scary type, but any reasonably polymorphic (say, over 'MonadIO') function that consumes a 'Stream' can go here.
         -> IO void
tailFile path consumer = System.IO.TailFile.Foldl.tailFile path (evertMIO consumer)
