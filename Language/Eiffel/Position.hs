{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Eiffel.Position
    (Pos (..)
    ,Line
    ,Column
    ,SourcePos

    ,sourceLine
    ,sourceColumn
    ,sourceName

    ,inheritPos

    ,attachPos
    ,attachPosM
    ,attachEmptyPos
    ,attachPosBefore
    ,attachPosHere

    ,takePos

    ,position
    ,contents
    ) where

import Control.Monad

import Data.DeriveTH
import Data.Binary
import Control.DeepSeq

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.ByteString

import qualified GHC.Generics as G

data Pos a = Pos SourceName !Line !Column a deriving (Ord, G.Generic)

instance Eq a => Eq (Pos a) where
    (==) p1 p2 = contents p1 == contents p2

instance Show a => Show (Pos a) where
    show p = -- show (position p) ++ "> " ++
             show (contents p)

instance Functor Pos where
    fmap f (Pos sn l c a) = Pos sn l c (f a)

inheritPos :: (Pos a -> b) -> Pos a -> Pos b
inheritPos f a = attachPos (position a) (f a)

takePos :: Pos a -> b -> Pos b
takePos pa = attachPos (position pa)

attachEmptyPos = attachPos (initialPos "<no file name>")

attachPos :: SourcePos -> a -> Pos a
attachPos s = Pos (sourceName s) (sourceLine s) (sourceColumn s)

attachPosM :: Monad m => m SourcePos -> m a -> m (Pos a)
attachPosM = liftM2 attachPos

attachPosHere :: a -> Parser (Pos a)
attachPosHere a = flip attachPos a `fmap` getPosition

attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = attachPosM getPosition

position :: Pos a -> SourcePos
position (Pos sn l c _) = newPos sn l c

contents :: Pos a -> a
contents (Pos _ _ _ a) = a

instance Binary SourcePos where
  get = return (newPos "filename lost" 0 0)
  put _p = return ()

-- instance Binary SourcePos where
--     get = do (line, col, name) <- get
--              return (newPos name line col)

--     put p = put (sourceLine p, sourceColumn p, sourceName p)

$( derive makeBinary ''Pos )

instance NFData SourcePos where
    rnf p = sourceLine p `seq` sourceColumn p `seq` sourceName p `seq` ()

$( derive makeNFData ''Pos )
