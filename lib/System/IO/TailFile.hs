module System.IO.TailFile (tailFile) where

import qualified Data.ByteString
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Control.Monad
import Control.Concurrent.QSem
import System.INotify
import System.IO (withFile,IOMode(ReadMode))

tailFile :: FilePath -> (a -> Data.ByteString.ByteString -> IO a) -> IO a -> IO void
tailFile filepath callback initial = withINotify $ \i -> do
    sem <- newQSem 1
    _ <- addWatch i [Modify] filepath (\_ -> signalQSem sem)
    state <- initial
    withFile filepath ReadMode (\h -> handleToStream sem h state)
    where
    handleToStream sem h =
        let go a = do waitQSem sem
                      a' <- readWithoutClosing h a
                      go a'
        in  go
    readWithoutClosing h = 
        let go a = do c <- Data.ByteString.hGetSome h defaultChunkSize
                      if Data.ByteString.null c
                         then do return a
                         else do a' <- callback a c
                                 readWithoutClosing h a'
        in  go
