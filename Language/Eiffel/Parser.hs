{-# LANGUAGE BangPatterns #-}
module Language.Eiffel.Parser where

import Control.Exception as E

import qualified Data.ByteString.Char8 as B (readFile)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)

import Language.Eiffel.Syntax
import Language.Eiffel.Parser.Class
import qualified Language.Eiffel.Parser.Lex as L
import Language.Eiffel.Parser.Statement

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.ByteString

newError name err = newErrorMessage (Message err) (newPos name 0 0)

lexThenParse :: L.Parser a -> String -> ByteString -> Either ParseError a
lexThenParse p name bstr = 
    let lexed = L.tokenizer name bstr -- parse L.tokenizer name bstr
    in case lexed of
         Left err -> Left (newError name err)
         Right tks -> parse p name tks

lexThenParseFromFile :: L.Parser a -> String -> IO (Either ParseError a)
lexThenParseFromFile p name = do 
    !lexed <- L.tokenizeFile name
    case lexed of
      Left err -> return $ Left $ newError name err
      Right tks -> return $ parse p name tks

countTokens :: String -> IO (Int)
countTokens name = do 
    lexed <- L.tokenizeFile name -- parseFromFile L.tokenizer name
    case lexed of
      Left _err -> return 0
      Right tks -> return $ length tks

parseStmt :: ByteString -> Either ParseError Stmt
parseStmt = lexThenParse stmt  ""

parseClass :: ByteString -> Either ParseError Clas
parseClass = lexThenParse clas ""

parseInterface :: ByteString -> Either ParseError ClasInterface
parseInterface = lexThenParse clasInterfaceP ""

parseClass' :: ByteString -> Clas
parseClass' = either (error . show) id . parseClass

parseFromName :: ClassName -> IO Clas
parseFromName cn = 
    either (error . show) return . parseClass =<< B.readFile (classNameFile cn)

classNameFile :: ClassName -> String
classNameFile cn = map toLower cn ++ ".e"

parseClassFile :: String -> IO (Either ParseError Clas)
parseClassFile = lexThenParseFromFile clas
