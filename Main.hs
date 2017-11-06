import Data.List
import Data.Maybe
import Data.Monoid
import Data.Default
import Control.Monad
import Control.Monad.Free
import Data.Functor.Rep

import Entities.Mark
import Entities.Coordinate
import Entities.Board
import Entities.Player

data Turn = Turn { player' :: Player, board' :: (Board Mark) } 

data Play next 
	= Start next
	| Ask (Coordinate () -> next)
	| Wrong (Coordinate ()) next
	| Loop Turn next
	| Final String

type Playing = Free Play

rules :: Playing ()
rules = Free $ Start $ gameloop (Turn Crosses (def :: Board Mark)) where

	gameloop :: Turn -> Playing ()
	gameloop turn = case check (board' turn) of
		Just E -> Free $ Loop turn $ continue turn
		Just O -> Free $ Loop turn $ Free $ Final "Noughts win!"
		Just X -> Free $ Loop turn $ Free $ Final "Crosses win!"
		Nothing -> if not $ ended (board' turn) 
			then Free $ Loop turn $ continue turn
			else Free $ Loop turn $ Free $ Final "Standoff here!"

	continue :: Turn -> Playing ()
	continue (Turn player board) = Free $ Ask $ \coordinate ->
		if index board coordinate /= E
		then Free $ Wrong coordinate $ gameloop $ Turn player board
		else gameloop $ Turn (another player) $
			change board (whose player) coordinate

	ended :: Board Mark -> Bool
	ended board = maybe True (const False) $ find (== E) $
		index board (I_IV ()) : index board (I_V ()) : index board (I_VI ()) :
		index board (II_IV ()) : index board (II_V ()) : index board (II_VI ()) :
		index board (III_IV ()) : index board (III_V ()) : index board (III_VI ()) : []

run :: Playing () -> IO ()
run (Pure r) = return r
run (Free (Start next)) = run next
run (Free (Ask f)) = print "Your turn: " >> ask >>= run . f
run (Free (Wrong coordinate next)) = print ((show coordinate) ++ " already filled") >> run next
run (Free (Loop turn next)) = print (board' turn) >> run next
run (Free (Final message)) = print message

main = run rules