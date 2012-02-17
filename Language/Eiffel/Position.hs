module Language.Eiffel.Position 
    (
     Pos ()
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

    ,position
    ,contents
    ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.ByteString

data Pos a = Pos SourcePos a deriving (Eq)

instance Show a => Show (Pos a) where
    show p = show (position p) ++ "> " ++ show (contents p)

instance Functor Pos where
    fmap f (Pos s a) = Pos s (f a)

inheritPos :: (Pos a -> b) -> Pos a -> Pos b
inheritPos f a = attachPos (position a) (f a)

attachEmptyPos = attachPos (initialPos "<no file name>")

attachPos :: SourcePos -> a -> Pos a
attachPos = Pos

attachPosM :: Monad m => m SourcePos -> m a -> m (Pos a)
attachPosM = liftM2 attachPos

attachPosHere :: a -> Parser (Pos a)
attachPosHere a = flip attachPos a `fmap` getPosition

attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = attachPosM getPosition

position :: Pos a -> SourcePos
position (Pos p _) = p

contents :: Pos a -> a
contents (Pos _ a) = a