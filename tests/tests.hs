{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
module Main where

import Data.Foldable
import Data.Monoid
import Data.ByteString(ByteString)
import Data.ByteString.Char8(unpack)
import qualified Data.ByteString as Bytes
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Exception
import Data.IORef
import Control.Concurrent.Conceit

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.FilePath((</>))
import System.IO
import System.IO.Error
import System.Process.Streaming
import System.IO.TailFile

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testCase "notExistingAtFirst" testNotExistingAtFirst
                          , testCase "preexisting" testPreexisting
                          , testCase "truncation" testTruncation
                          , testCase "move" testMove
                          ]

testPreexisting :: IO ()
testPreexisting = 
  do (filename1,_) <- deleteFiles
     Bytes.writeFile filename1 "previous content\n"
     let content1 = "1 new content"
         content2 = "2 new content"
     bytes <- tailToIORef (\filepath -> 
                              do catToFile filepath content1
                                 halfsec
                                 catToFile filepath content2)
                          filename1
     assertEqual "" (newlines [content1,content2]) bytes

testTruncation :: IO ()
testTruncation = 
  do (filename1,_) <- deleteFiles
     Bytes.writeFile filename1 "previous content\n"
     let content1 = "1 new content"
         content2 = "2 new content"
     bytes <- tailToIORef (\filepath -> 
                              do catToFile filepath content1
                                 halfsec
                                 truncateFile filepath
                                 halfsec
                                 catToFile filepath content2)
                          filename1
     assertEqual "" (newlines [content1,content2]) bytes

testMove :: IO ()
testMove = 
  do (filename1,filename2) <- deleteFiles
     let content1 = "1 new content"
         content2 = "2 new content"
     bytes <- tailToIORef (\filepath -> 
                              do catToFile filepath content1
                                 halfsec
                                 renameFile filepath filename2
                                 halfsec
                                 catToFile filepath content2
                                 halfsec
                                 halfsec
                                 halfsec)
                          filename1
     assertEqual "" (newlines [content1,content2]) bytes

testNotExistingAtFirst :: IO ()
testNotExistingAtFirst = 
  do (filename1,_) <- deleteFiles
     let content1 = "1 new content"
         content2 = "2 new content"
     bytes <- tailToIORef (\filepath -> 
                              do catToFile filepath content1
                                 halfsec
                                 catToFile filepath content2
                                 halfsec
                                 halfsec)
                          filename1
     assertEqual "" (newlines [content1,content2]) bytes

truncateFile :: FilePath -> IO ()
truncateFile filepath =
    execute (piped (shell ("truncate -s 0 "<> filepath))) 
            (pure ())

catToFile :: FilePath -> ByteString -> IO ()
catToFile filepath content =
    execute (piped (shell ("echo \"" <> unpack content <> "\" >> " <> filepath))) 
            (pure ())
       
halfsec :: IO ()
halfsec = threadDelay 5e5

newlines :: [ByteString] -> ByteString
newlines bs = mconcat . map (\b -> b <> "\n") $ bs

deleteFiles :: IO (FilePath,FilePath)
deleteFiles = do
    tmp <- getTemporaryDirectory  
    let (basename1,basename2) = ("haskell_tailfile_test_1_387493423492347.txt"
                                ,"haskell_tailfile_test_2_387493423492347.txt")
        (filename1,filename2) = (tmp </> basename1,tmp </> basename2)
    for_ [filename1,filename2]
         (\filepath -> do _ <- tryJust (guard . isDoesNotExistError) 
                                       (removeFile filepath)
                          pure ())
    return (filename1,filename2)

tailToIORef :: (FilePath -> IO ()) -> FilePath -> IO Data.ByteString.ByteString
tailToIORef writer filepath =
  do ref <- newIORef mempty
     let addToRef _ bytes = modifyIORef' ref (\b -> b <> bytes) 
     runConceit (Conceit (Left <$> (halfsec *> halfsec *> writer filepath))    
                 *> 
                 _Conceit (tailFile filepath addToRef (pure ())))
     readIORef ref 
