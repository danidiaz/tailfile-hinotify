{-| Tail files in Unix, using types from the @pipes@ package.

 -}

{-# language RankNTypes #-}
module System.IO.TailFile.Pipes where

import           Data.ByteString (ByteString)
import           Pipes
import           Streaming.Eversion.Pipes
import qualified System.IO.TailFile.Foldl

{-| Tail a file with a function that consumes a 'Producer'.
-}
tailFile :: FilePath
         -> (forall t r. (MonadTrans t, MonadIO (t IO)) => Producer Data.ByteString.ByteString (t IO) r -> t IO (void, r)) -- ^ Scary type, but any resonably polymorphic (say, over 'MonadIO') function that consumes a 'Producer' can go here.
         -> IO void
tailFile path consumer = System.IO.TailFile.Foldl.tailFile path (evertMIO consumer)
