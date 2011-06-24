module Types where

import Control.Monad
import qualified Control.Monad.State as MST (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO, MonadIO(..)) 
import qualified Data.Map as M

data AppType = LeftApp | RightApp
	deriving (Eq, Show, Read)

type Vitality = Int
data Function = FValue Int
				| FFunc CardFunction

type Field = Function
data Slot = Slot Vitality Field

type Slots = M.Map Int Slot

data Player = Player0 | Player1
	deriving (Eq, Show, Read)
	
data GameData = GD {propSlots :: Slots,
					opSlots :: Slots,
					curPlayer :: Player}

type GS a = (MST.StateT GameData Maybe a)

type CardFunction = [Maybe Function] -> GS (Maybe Function)

defaultSlot = Slot 10000 (FFunc fI)
defaultSlots = zip [0..255] (replicate 256 defaultSlot)

isSlotAlive (Slot v _) = v > 0


dummyGameData = GD {propSlots = M.fromList defaultSlots,
					opSlots   = M.fromList defaultSlots,
					curPlayer = Player0}

currentPlayerSlots (GD slots _ Player0) = slots 
currentPlayerSlots (GD _ slots Player1) = slots


field :: Int -> Slots -> Bool -> Maybe Function
field i slots isAlive = do
			case M.lookup i slots of
				Just sl@(Slot v field) -> case isSlotAlive sl == isAlive of
											True -> Just field
											False -> Nothing
				Nothing -> Nothing

fZero :: [Maybe Function] -> GS (Maybe Function)
fZero [] = return $ Just $ FValue 0
fZero _  = return $ Nothing

fI :: [Maybe Function] -> GS (Maybe Function)
fI (Just func:[]) = return $ Just $ func
fI _ = return Nothing

fSucc :: [Maybe Function] -> GS (Maybe Function)
fSucc (Just (FValue x):[]) = return $ Just $ FValue (x + 1)
fSucc _ = return Nothing

fDbl :: [Maybe Function] -> GS (Maybe Function)
fDbl (Just (FValue x):[]) = return $ Just $ FValue (x * 2)
fDbl _ = return Nothing

fGet :: [Maybe Function] -> GS (Maybe Function)
fGet (Just (FValue i):[]) =
		do
			gd <- MST.get
			return $ field i (currentPlayerSlots gd) True
fGet _ = return Nothing

fPut :: [Maybe Function] -> GS (Maybe Function)
fPut (_:[]) = do
				x <- fI []
				return x

fS :: [Maybe Function] -> GS (Maybe Function)
fS (Just (FFunc f):Just (FFunc g):x:[]) = do
					h <- f [x]
					y <- g [x]
					case h of
						Just (FFunc h') -> h' [y]
						Nothing -> return Nothing


test1 = do
			x <- fZero []
			fI [x]
test2 = [fI, fZero, fGet, fSucc]