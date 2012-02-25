module Main where

import Control.Exception as E
import Control.Monad

import qualified Data.ByteString.Char8 as BS
import Data.List

import Language.Eiffel.PrettyPrint
import Language.Eiffel.Parser.Parser

import System.Directory
import System.FilePath

relativePaths = [".", ".."]

testDirectory dir = do
  allFiles <- getDirectoryContents dir
  let eFiles = filter ((== ".e") . snd . splitExtension) allFiles
  return $ map (combine dir) eFiles

allTestFiles :: IO [FilePath]
allTestFiles = do
  pwd <- getCurrentDirectory
  subdirs <- getDirectoryContents pwd
  subdirs' <- filterM doesDirectoryExist (subdirs \\ relativePaths)
  fileNames <- mapM testDirectory subdirs'
  return (concat fileNames)

parseAndPrint fileName = 
    let parse bstr = case parseClass (BS.pack bstr) of
                            Left e -> error (show e)
                            Right c -> c
    in do
      content <- readFile fileName
      (print . toDoc . parse) content
  
test content = 
    let parse pass bstr = case parseClass (BS.pack bstr) of
                            Left e -> error ("Error in pass " ++ show pass ++ ": " ++ show e)
                            Right c -> c
        roundTrip = (parse 2) . show . toDoc . (parse 1)
    in (parse 1) content == roundTrip content

testFile fileName = do
  str <- readFile fileName
  let response = do
        pass <- evaluate $ test str
        if pass 
          then putStrLn $ "Passed: " ++ fileName
          else putStrLn $ "Failed: " ++ fileName ++ ", ASTs differ"
  E.catch response
          ( \ (ErrorCall s) -> putStrLn $ "Failed: " ++ fileName ++ ", parsing failed with:\n" ++ s)
main = do
  allTestFiles >>= mapM_ testFile