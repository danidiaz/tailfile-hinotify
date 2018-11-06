{-| Tail files in Unix, using folds form the @foldl@ package.

 -}
module System.IO.TailFile.Foldl where

import           Data.ByteString (ByteString)
import qualified Control.Foldl as L
import qualified System.IO.TailFile

{-| Like 'System.IO.TailFile.tailFile', but it takes a 'L.FoldM'.

    The @done@ part of the fold is never invoked.
 -}
tailFile :: FilePath
         -> L.FoldM IO Data.ByteString.ByteString void -> IO void
tailFile path = L.impurely (\step initial _ -> System.IO.TailFile.tailFile path step initial)
