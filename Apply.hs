module Apply where

import Control.Monad
import qualified Control.Monad.State as MST (get, gets, StateT(..), evalStateT, 
                            put, MonadState(..), liftIO, MonadIO(..))
import qualified Data.Map as M

import Types
import Constructing

defaultSlot :: Slot
defaultSlot = Slot 10000 (FFunc fI)

defaultSlots :: [(Int, Slot)]
defaultSlots = zip [0..255] (replicate 256 defaultSlot)

getField i slots isAlive = do
			case M.lookup i slots of
				Just sl@(Slot _ field) -> case isSlotAlive sl == isAlive of
											True -> Just field
											False -> Nothing
				Nothing -> Nothing

getVitality i slots isAlive = do
			case M.lookup i slots of
				Just sl@(Slot v _) -> case isSlotAlive sl == isAlive of
											True -> Just $ FValue v
											False -> Nothing
				Nothing -> Nothing

setVitality i (FValue newVit) slots gd pl = case M.lookup i slots of
			Just (Slot _ f) -> let updatedSl =  M.update (\x -> Just (Slot newVit f)) i slots in
				Just $ setSlots gd pl updatedSl
			Nothing -> Nothing

setSlot_ i isAlive slot slots gd pl = case M.lookup i slots of
			Just oldSlot -> case isSlotAlive oldSlot == isAlive of
								True -> let updatedSl =  M.update (\x -> Just slot) i slots in
										Just $ setSlots gd pl updatedSl
								False -> Nothing
			Nothing -> Nothing

field :: Int -> GameData -> Bool -> Maybe Function
field i (GD slots _ Player0) isAlive = getField i slots isAlive
field i (GD _ slots Player1) isAlive = getField i slots isAlive

field' :: Int ->  GameData -> Bool -> Maybe Function
field' i (GD _ slots Player0) isAlive = getField i slots isAlive
field' i (GD slots _ Player1) isAlive = getField i slots isAlive

vitality :: Int -> GameData -> Bool -> Maybe Function
vitality i (GD slots _ Player0) isAlive = getVitality i slots isAlive
vitality i (GD _ slots Player1) isAlive = getVitality i slots isAlive

vitality' :: Int -> GameData -> Bool -> Maybe Function
vitality' i (GD _ slots Player0) isAlive = getVitality i slots isAlive
vitality' i (GD slots _ Player1) isAlive = getVitality i slots isAlive

setSlotVitality :: Int -> Function -> GameData -> Maybe GameData
setSlotVitality i newVit gd@(GD slots _ Player0) = setVitality i newVit slots gd Player0
setSlotVitality i newVit gd@(GD _ slots Player1) = setVitality i newVit slots gd Player1

setSlotVitality' :: Int -> Function -> GameData -> Maybe GameData
setSlotVitality' i newVit gd@(GD _ slots Player0) = setVitality i newVit slots gd Player0
setSlotVitality' i newVit gd@(GD slots _ Player1) = setVitality i newVit slots gd Player1

setSlot :: Int -> Bool -> Slot -> GameData -> Maybe GameData
setSlot i isAlive newSlot gd@(GD slots _ Player0) = setSlot_ i isAlive newSlot slots gd Player0
setSlot i isAlive newSlot gd@(GD _ slots Player1) = setSlot_ i isAlive newSlot slots gd Player1

setSlot' :: Int -> Bool -> Slot -> GameData -> Maybe GameData
setSlot' i isAlive newSlot gd@(GD _ slots Player0) = setSlot_ i isAlive newSlot slots gd Player0
setSlot' i isAlive newSlot gd@(GD slots _ Player1) = setSlot_ i isAlive newSlot slots gd Player1

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
			return $ field i gd True
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

fK :: [Maybe Function] -> GS (Maybe Function)
fK (x:_:[]) = return x
fK _ = return Nothing

fInc :: [Maybe Function] -> GS (Maybe Function)
fInc (Just (FValue i):[]) = do
				gd <- MST.get
				let oldVit = vitality i gd True
				let maybeNewGD = oldVit >>= (\(FValue v) -> if v < 65535 then Just $ FValue $ v + 1 else Just $ FValue v) >>= (\y -> setSlotVitality i y gd)
				case maybeNewGD of
					Just newGD -> do
									MST.put newGD
									x <- fI []
									return x
					Nothing -> return Nothing
fInc _ = return Nothing

fDec :: [Maybe Function] -> GS (Maybe Function)
fDec (Just (FValue i):[]) = do
				gd <- MST.get
				let oldVit = vitality' (255-i) gd True
				let maybeNewGD = oldVit >>= (\(FValue v) -> if v > 0 then Just $ FValue $ v - 1 else Just $ FValue v) >>= (\y -> setSlotVitality' (255-i) y gd)
				case maybeNewGD of
					Just newGD -> do
									MST.put newGD
									x <- fI []
									return x
					Nothing -> return Nothing
fDec _ = return Nothing

fAttack :: [Maybe Function] -> GS (Maybe Function)
fAttack (Just (FValue i):Just (FValue j):(Just (FValue n)):[]) = do
				gd <- MST.get
				let oldPropVit = vitality i gd True
				let maybeNewPropGD = oldPropVit >>= (\(FValue v) -> if n > v then Nothing else Just $ FValue $ v - n) >>= (\y -> setSlotVitality i y gd)
				case maybeNewPropGD of
					Just newPropGD -> do
						let oldOpVit = vitality' (255-j) newPropGD True
						let divided = div (n * 9) 10
						let maybeNewOpGD = oldOpVit >>= (\(FValue v) -> if v - divided < 0 then Just $ FValue 0 else Just $ FValue $ v - divided) >>= (\y -> setSlotVitality' (255-j) y newPropGD)
						case maybeNewOpGD of
							Just newOpGD -> do
								MST.put newOpGD
								x <- fI []
								return x
							Nothing -> return Nothing
					Nothing -> return Nothing
fAttack _ = return Nothing

fHelp :: [Maybe Function] -> GS (Maybe Function)
fHelp (Just (FValue i):Just (FValue j):(Just (FValue n)):[]) = do
				gd <- MST.get
				let oldPropVit1 = vitality i gd True
				let maybeNewPropGD1 = oldPropVit1 >>= (\(FValue v) -> if n > v then Nothing else Just $ FValue $ v - n) >>= (\y -> setSlotVitality i y gd)
				case maybeNewPropGD1 of
					Just newPropGD1 -> do
						let oldPropVit2 = vitality j newPropGD1 True
						let divided = div (n * 11) 10
						let maybeNewPropGD2 = oldPropVit2 >>= (\(FValue v) -> if v + divided > 65535 then Just $ FValue 65535 else Just $ FValue $ v + divided) >>= (\y -> setSlotVitality j y newPropGD1)
						case maybeNewPropGD2 of
							Just newPropGD2 -> do
								MST.put newPropGD2
								x <- fI []
								return x
							Nothing -> return Nothing
					Nothing -> return Nothing
fHelp _ = return Nothing

fCopy ::  [Maybe Function] -> GS (Maybe Function)
fCopy (Just (FValue i):[]) = do
				gd <- MST.get
				return $ field' i gd True
fCopy _ = return Nothing

fRevive :: [Maybe Function] -> GS (Maybe Function)
fRevive (Just (FValue i):[]) = do
				gd <- MST.get
				let oldPropVit = vitality i gd False
				let maybeNewGD = oldPropVit >>= \_ -> setSlotVitality i (FValue 1) gd
				case maybeNewGD of
					Just ngd -> do
									MST.put ngd
									x <- fI []
									return x
					Nothing -> return Nothing
fRevive _ = return Nothing

fZombie :: [Maybe Function] -> GS (Maybe Function)
fZombie (Just (FValue i):Just x:[]) = do
				gd <- MST.get
				case setSlot i False (Slot (-1) x) gd of
					Just newGD -> do
									MST.put newGD
									y <- fI []
									return y
					Nothing -> return Nothing
fZombie _ = return Nothing
