
module Main where

import Types
import Apply
import Constructing

import Control.Monad
import qualified Control.Monad.State as MST (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO, MonadIO(..))
import qualified Data.Map as M

showField :: Function -> String
showField (FValue 0) = "zero"
showField (FValue n) = show n
showField (FFunc func) = []

showSlotList :: SlotList -> String
showSlotList [] = "\n(slots {10000,I} are omitted)"
showSlotList ((_, slot):xs) | isDefaultSlot slot = showSlotList xs
showSlotList ((n, Slot v f):xs) = "\n" ++ show n ++ "={" ++ show v ++ "," ++ showField f ++ "}" ++ showSlotList xs

showSlots :: Slots -> String
showSlots = showSlotList . M.toList



sh :: CardFunction -> String
sh cf = case cf [] of
			x -> undefined


initGameData = GD {propSlots = M.fromList defaultSlots,
					opSlots   = M.fromList defaultSlots,
					curPlayer = Player0}


showGS :: GS (Maybe Function) -> GS (String)
showGS gs = do
				maybeFunc <- gs
				case maybeFunc of
					Just f -> return ""
					Nothing -> return ""

test1 = do
			x <- fZero []
			fI [x]
test2 = [fI, fZero, fSucc, fGet, fPut, fS, fK, fInc, fDec, fAttack, fHelp, fCopy, fRevive, fZombie]

test3 :: Applications
test3 = [(0, SlotToCard, fZero), (0, CardToSlot, fSucc), (0, CardToSlot, fDbl)]

