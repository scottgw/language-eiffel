{-# LANGUAGE BangPatterns #-}
module Language.Eiffel.Parser where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Text (Text)
import           Data.Char (toLower)

import           Language.Eiffel.Syntax
import           Language.Eiffel.Parser.Class
import qualified Language.Eiffel.Parser.Lex as L
import           Language.Eiffel.Parser.Statement

import           Text.Parsec
import           Text.Parsec.Error
import           Text.Parsec.Pos

newError name err = newErrorMessage (Message err) (newPos name 0 0)

lexThenParse :: L.Parser a -> String -> Text -> Either ParseError a
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

parseStmt :: Text -> Either ParseError Stmt
parseStmt = lexThenParse stmt  ""

parseClass :: Text -> Either ParseError Clas
parseClass = lexThenParse clas ""

parseInterface :: Text -> Either ParseError ClasInterface
parseInterface = lexThenParse clasInterfaceP ""

parseClass' :: Text -> Clas
parseClass' = either (error . show) id . parseClass

parseFromName :: ClassName -> IO Clas
parseFromName cn = 
    either (error . show) return . parseClass =<< 
    Text.readFile (classNameFile cn)

classNameFile :: ClassName -> String
classNameFile cn = Text.unpack (Text.toLower cn) ++ ".e"

parseClassFile :: String -> IO (Either ParseError Clas)
parseClassFile = lexThenParseFromFile clas
