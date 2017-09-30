module Entities.Player (Player(..), another, whose) where

import Entities.Mark

data Player = Noughts | Crosses deriving Show

another :: Player -> Player
another Noughts = Crosses
another Crosses = Noughts

whose :: Player -> Mark
whose Noughts = O
whose Crosses = X