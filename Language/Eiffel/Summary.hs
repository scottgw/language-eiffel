module Language.Eiffel.Summary where

import qualified Data.ByteString.Char8 as B

import Language.Eiffel.Syntax
import Language.Eiffel.Parser.Class
import qualified Language.Eiffel.Parser.Lex as L
import Language.Eiffel.Parser.Parser
import Language.Eiffel.PrettyPrint

import Text.Parsec

import System.IO

summaryP :: L.Parser [ClasInterface]
summaryP = many clasInterfaceP

parseSummary :: String -> IO (Either ParseError [ClasInterface])
parseSummary fileName = lexThenParseFromFile summaryP fileName

writeSummary :: FilePath -> [ClasInterface] -> IO ()
writeSummary filePath ifaces = 
  withFile filePath WriteMode $ \ hdl ->
    mapM_ (B.hPutStrLn hdl . B.pack . show . toInterfaceDoc) ifaces