module System.IO.TailFile.Foldl where

import qualified Data.ByteString
import qualified Control.Foldl as L
import qualified System.IO.TailFile

tailFile :: FilePath -> L.FoldM IO Data.ByteString.ByteString void -> IO void
tailFile path = L.impurely (\step initial _ -> System.IO.TailFile.tailFile path step initial)
