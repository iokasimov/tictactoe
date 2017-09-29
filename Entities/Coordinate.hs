module Entities.Coordinate (Coordinate(..), Row(..), Column(..), ask) where

import Data.Default

data Row = I | II | III deriving Show
data Column = IV | V | VI deriving Show

data Coordinate a = Coordinate Row Column a deriving Show

instance Functor Coordinate where
	fmap f (Coordinate row column x) =
		Coordinate row column $ f x

ask :: IO (Coordinate ())
ask = getChar >>= \char -> case char of
	'1' -> pure $ Coordinate I IV ()
	'2' -> pure $ Coordinate I V ()
	'3' -> pure $ Coordinate I VI ()
	'4' -> pure $ Coordinate II IV ()
	'5' -> pure $ Coordinate II V ()
	'6' -> pure $ Coordinate II VI ()
	'7' -> pure $ Coordinate III IV ()
	'8' -> pure $ Coordinate III V ()
	'9' -> pure $ Coordinate III VI ()
	_ -> ask