module Main where

import Control.Exception as E
import Control.Monad

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.List

import Language.Eiffel.PrettyPrint
import Language.Eiffel.Parser

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
    let parse bstr = case parseClass (Text.pack bstr) of
                            Left e -> error (show e)
                            Right c -> c
    in do
      content <- readFile fileName
      (print . toDoc . parse) content

test content =
    let parse pass bstr = case parseClass bstr of
                            Left e -> error ("Error in pass " ++ show pass ++ ": " ++ show e)
                            Right c -> c
        roundTrip = parse 2 . Text.pack . show . toDoc
        parse1 = parse 1 content
    in if parse1 == roundTrip parse1
          then Nothing
          else Just (parse1, roundTrip parse1)

data TestResult = Passed FilePath
                | FailedDiffer FilePath
                | Failed FilePath String

testFile fileName = do
  str <- TextIO.readFile fileName
  let response = do
        pass <- evaluate $ test str
        case pass of
          Nothing -> return (Passed fileName)
          Just (parse1, parse2) -> do
            writeFile (fileName ++ "1") (show $ toDoc parse1)
            writeFile (fileName ++ "2") (show $ toDoc parse2)
            return (FailedDiffer fileName)
  E.catch response
          ( \ (ErrorCall s) -> return (Failed fileName s))

isPass (Passed _) = True
isPass _ = False

isDiffer (FailedDiffer _) = True
isDiffer _ = False

testFile2 filename = do
  clsEi <- parseClassFile filename
  case clsEi of
    Left e  -> return $ Failed filename (show e)
    Right _ -> return $ Passed filename

report rs =
  let reportSingle (Failed file reason) = putStrLn (file ++ ": Failed with\n " ++ reason)
      reportSingle (FailedDiffer file)  = putStrLn (file ++ ": Failed with differing ASTs")
      reportSingle (Passed file)        = putStrLn (file ++ ": Passed")
      passFail = show (length $ filter isPass rs) ++ "/" ++ show (length $ filter isDiffer rs) ++ "/" ++ show (length rs)
  in do mapM reportSingle rs >> putStrLn passFail

main = do
  pwd <- getCurrentDirectory
  setCurrentDirectory (pwd ++ "/tests")
  allTestFiles >>= mapM testFile >>= report
