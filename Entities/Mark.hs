module Entities.Mark (Mark(..)) where

import Data.Default

data Mark = E | X | O deriving Eq

instance Show Mark where
	show E = "_"
	show X = "x"
	show O = "o"

instance Default Mark
	where def = E