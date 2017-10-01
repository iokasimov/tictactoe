module Entities.Board (Board(..), top, middle, bottom, change, Entities.Board.check) where

import Data.Default
import Data.Distributive
import Data.Functor.Rep
import Data.Functor.Adjunction
import Control.Lens hiding (index)

import Entities.Coordinate
import Entities.Line

data Board a = Board (Line a) (Line a) (Line a) deriving Eq

instance Show a => Show (Board a) where
	show board = "---------------------- \n"
		++ "    IV  V  VI \n"
		++ "I   " ++ show (board ^. top) ++ "\n"
		++ "II  " ++ show (board ^. middle) ++ "\n"
		++ "III " ++ show (board ^. bottom) ++ "\n"
		++ "---------------------- \n"

instance Default a => Default (Board a) where
	def = tabulate $ \_ -> def

instance Functor Board where
	fmap f (Board top middle bottom) = 
		Board (f <$> top) (f <$> middle) (f <$> bottom)

instance Distributive Board where
	distribute = distributeRep

instance Representable Board where
	type Rep Board = Coordinate ()

	index board (Coordinate I IV _) = board ^. (top . first)
	index board (Coordinate I V _) = board ^. (top . second)
	index board (Coordinate I VI _) = board ^. (top . third)
	index board (Coordinate II IV _) = board ^. (middle . first)
	index board (Coordinate II V _) = board ^. (middle . second)
	index board (Coordinate II VI _) = board ^. (middle . third)
	index board (Coordinate III IV _) = board ^. (bottom . first)
	index board (Coordinate III V _) = board ^. (bottom . second)
	index board (Coordinate III VI _) = board ^. (bottom . third)

	tabulate desc = Board
		(Line (desc $ Coordinate I IV (), desc $ Coordinate I V (), desc $ Coordinate I VI ()))
		(Line (desc $ Coordinate II IV (), desc $ Coordinate II V (), desc $ Coordinate II VI ()))
		(Line (desc $ Coordinate III IV (), desc $ Coordinate III V (), desc $ Coordinate III VI ()))

instance Adjunction Coordinate Board where
	unit a = tabulate (\(Coordinate row col ()) -> Coordinate row col a)
	counit (Coordinate row col board) = index board (Coordinate row col ())

top :: Lens' (Board a) (Line a)
top modifier (Board a b c) = (\x -> Board x b c) <$> modifier a

middle :: Lens' (Board a) (Line a)
middle modifier (Board a b c) = (\x -> Board a x c) <$> modifier b

bottom :: Lens' (Board a) (Line a)
bottom modifier (Board a b c) = (\x -> Board a b x) <$> modifier c

change :: Board a -> a -> Coordinate () -> Board a
change board m (Coordinate I IV ()) = board & (top . first) .~ m
change board m (Coordinate I V ()) = board & (top . second) .~ m
change board m (Coordinate I VI ()) = board & (top . third) .~ m
change board m (Coordinate II IV ()) = board & (middle . first) .~ m
change board m (Coordinate II V ()) = board & (middle . second) .~ m
change board m (Coordinate II VI ()) = board & (middle . third) .~ m
change board m (Coordinate III IV ()) = board & (bottom . first) .~ m
change board m (Coordinate III V ()) = board & (bottom . second) .~ m
change board m (Coordinate III VI ()) = board & (bottom . third) .~ m

-- how this piece of ugly code can be reduced?
check :: Eq a => Board a -> Maybe a
check board = case Entities.Line.check $ board ^. top of
	Nothing -> case Entities.Line.check $ board ^. middle of
		Nothing -> case Entities.Line.check $ board ^. bottom of
			Nothing -> case Entities.Line.check $ Line (board ^. (top . first), board ^. (middle . first), board ^. (bottom . first)) of
				Nothing -> case Entities.Line.check $ Line (board ^. (top . second), board ^. (middle . second), board ^. (bottom . second)) of
					Nothing -> case Entities.Line.check $ Line (board ^. (top . third), board ^. (middle . third), board ^. (bottom . third)) of
						Nothing -> case Entities.Line.check $ Line (board ^. (top . first), board ^. (middle . second), board ^. (bottom . third)) of
							Nothing -> case Entities.Line.check $ Line (board ^. (top . third), board ^. (middle . second), board ^. (bottom . first)) of
								Nothing -> Nothing
								Just h -> Just h
							Just g -> Just g
						Just f -> Just f
					Just e -> Just e		
				Just d -> Just d
			Just c -> Just c
		Just b -> Just b
	Just a -> Just a
