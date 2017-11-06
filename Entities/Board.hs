module Entities.Board (Board(..), 
	pattern I_IV, pattern I_V, pattern I_VI,
	pattern II_IV, pattern II_V, pattern II_VI,
	pattern III_IV, pattern III_V, pattern III_VI,
	top, middle, bottom, 
	change, Entities.Board.check
) where

import Data.Monoid
import Data.Default
import Data.Foldable
import Data.Distributive
import Data.Functor.Rep
import Data.Functor.Adjunction
import Control.Lens hiding (index)

import Entities.Coordinate
import Entities.Line

data Board a = Board (Line a) (Line a) (Line a) deriving Eq

pattern I_IV x = Coordinate I IV x
pattern I_V x = Coordinate I V x
pattern I_VI x = Coordinate I VI x
pattern II_IV x = Coordinate II IV x
pattern II_V x = Coordinate II V x
pattern II_VI x = Coordinate II VI x
pattern III_IV x = Coordinate III IV x
pattern III_V x = Coordinate III V x
pattern III_VI x = Coordinate III VI x

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

	index board (I_IV _) = board ^. (top . first)
	index board (I_V _) = board ^. (top . second)
	index board (I_VI _) = board ^. (top . third)
	index board (II_IV _) = board ^. (middle . first)
	index board (II_V _) = board ^. (middle . second)
	index board (II_VI _) = board ^. (middle . third)
	index board (III_IV _) = board ^. (bottom . first)
	index board (III_V _) = board ^. (bottom . second)
	index board (III_VI _) = board ^. (bottom . third)

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
change board m (I_IV ()) = board & (top . first) .~ m
change board m (I_V ()) = board & (top . second) .~ m
change board m (I_VI ()) = board & (top . third) .~ m
change board m (II_IV ()) = board & (middle . first) .~ m
change board m (II_V ()) = board & (middle . second) .~ m
change board m (II_VI ()) = board & (middle . third) .~ m
change board m (III_IV ()) = board & (bottom . first) .~ m
change board m (III_V ()) = board & (bottom . second) .~ m
change board m (III_VI ()) = board & (bottom . third) .~ m

check :: Eq a => Board a -> Maybe a
check board = getFirst $ fold $ (First . Entities.Line.check) <$>
	(board ^. top) : (board ^. middle) : (board ^. bottom) : -- checking rows
	Line (board ^. (top . first), board ^. (middle . first), board ^. (bottom . first)) : -- first column 
	Line (board ^. (top . second), board ^. (middle . second), board ^. (bottom . second)) : -- second column
	Line (board ^. (top . third), board ^. (middle . third), board ^. (bottom . third)) : -- third column
	Line (board ^. (top . first), board ^. (middle . second), board ^. (bottom . third)) : -- main diagonal
	Line (board ^. (top . third), board ^. (middle . second), board ^. (bottom . first)) : [] -- minor diagonal