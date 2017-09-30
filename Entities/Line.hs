module Entities.Line (Line(..), first, second, third, check) where

import Control.Lens

data Line a = Line (a, a, a) deriving Eq

instance Show a => Show (Line a) where
	show (Line (a, b, c)) = show a ++ "   " ++ show b ++ "   " ++ show c

instance Functor Line where
	fmap f (Line (a, b, c)) = Line (f a, f b, f c)

first :: Lens' (Line a) a
first modifier (Line (a,b,c)) = 
	(\x -> Line (x,b,c)) <$> modifier a

second :: Lens' (Line a) a
second modifier (Line (a,b,c)) = 
	(\x -> Line (a,x,c)) <$> modifier b

third :: Lens' (Line a) a
third modifier (Line (a,b,c)) = 
	(\x -> Line (a,b,x)) <$> modifier c

check :: Eq a => Line a -> Maybe a
check line = if (line ^. first == line ^. second) 
	&& (line ^. second == line ^. third)
	then Just $ line ^. first else Nothing 
