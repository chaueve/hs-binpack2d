
module Data.BinPack2D
	( Position(..)
	, Size(..)
	, Bin()
	, emptyBin
	, pack
	, BinArray(..)
	, emptyBinArray
	, packArray
	) where

import Control.Applicative
import Control.Monad

import Data.Ord
import Data.Maybe
import Data.Semigroup
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Discrete position.
data Position
	= Position
	{ positionX :: !Word
	, positionY :: !Word
	} deriving (Eq)
instance Show Position where
	show (Position x y) = "(Position " ++ show x ++ ' ' : show y ++ ")"
instance Ord Position where
	Position x0 y0 `compare` Position x1 y1 = compare y0 y1 <> compare x0 x1

-- | Discrete size.
data Size
	= Size
	{ sizeWidth  :: !Word
	, sizeHeight :: !Word
	} deriving (Eq)
instance Show Size where
	show (Size w h) = "(Size " ++ show w ++ ' ' : show h ++ ")"
instance Ord Size where
	Size w0 h0 `compare` Size w1 h1 = compare h0 h1 <> compare w0 w1

-- | Axis aligned two dimensional bounded rectangular bin from which axis aligned
-- rectangles can be allocated.
data Bin
	= Bin
	{ binSize       :: !Size
	, binSkyline    :: [Position]
	, binGuillotine :: Map Position Size
	} deriving (Show)

-- | Empty 'Bin' with the given size.
emptyBin :: Size -> Bin
emptyBin size = Bin
	{ binSize       = size
	, binSkyline    = [Position 0 0]
	, binGuillotine = mempty
	}

-- | Internal monoid for 'pack'.
data PackM w
	= EmptyPackM
	| PackM
	{ packMwastage  :: !w
	, packMposition :: !Position
	, packMbin      :: Bin
	}
instance Ord w => Semigroup (PackM w) where
	EmptyPackM       <> (!b)             = b
	a                <> EmptyPackM       = a
	a@(PackM wa _ _) <> b@(PackM wb _ _)
		| wa <= wb  = a
		| otherwise = b
instance Ord w => Monoid (PackM w) where
	mempty = EmptyPackM

-- | 'PackM' to 'Maybe'.
fromPackM :: PackM w -> Maybe (Position, Bin)
fromPackM (PackM _ pos bin) = Just (pos, bin)
fromPackM _                 = Nothing

-- | Allocate a new rectangle from a 'Bin'.
pack
	:: Size -- ^ The requested size of the rectangle.
	-> Bin  -- ^ The 'Bin' from which to allocate.
	-> Maybe (Position, Bin) -- ^ The bottom left coordinate of the allocated rectangle and the new 'Bin'.
pack (Size 0  _ ) bin@Bin{..} = Nothing
pack (Size _  0 ) bin@Bin{..} = Nothing
pack (Size rw rh) bin@Bin{..}
	| rw > sizeWidth binSize || rh > sizeHeight binSize = Nothing
	| otherwise = guillotine <|> skyline where
	-- Given starting position and skyline tail,
	-- calculate y coordinate fitting the rectangle of size size
	-- into the skyline tail and bin size, if such fit exists.
	skyFit :: Position -> [Position] -> Maybe Word
	skyFit (Position x y) ps
		= if x+rw > sizeWidth binSize || y+rh > sizeHeight binSize
			then Nothing
			else skyFit1 (x+rw) y ps
	skyFit1 l py0 [] = Just py0
	skyFit1 l py0 (Position x y : ps)
		| x >= l    = Just py0
		| otherwise =
			let py1 = max py0 y
			in if py1 + rh > sizeHeight binSize then Nothing else skyFit1 l py1 ps

	-- Given position and skyline tail, calculate
	-- the new skyline tail. The rectangle is assumed to fit.
	skyTail :: Position -> [Position] -> [Position]
	skyTail (Position px py) ps = Position px (py+rh) : skyTail1 (px+rw) py ps
	skyTail1 l y0 ps0@(Position x y:ps1)
		| x == l    = ps0
		| x > l     = Position l y0 : ps0
		| otherwise = skyTail1 l y ps1
	skyTail1 l y0 [] = [Position l 0]

	-- Given reversed skyline init and skyline tail,
	-- glue them together.
	skyGlue rps0@(Position rpx rpy : rps1) ps0@(Position px py : ps1)
		| rpy == py = skyGlue1 rps1 $ Position rpx rpy : ps1
		| otherwise = skyGlue1 rps0 ps0
	skyGlue rps ps = skyGlue1 rps ps
	skyGlue1 (p:rps) ps = skyGlue1 rps (p:ps)
	skyGlue1 [] ps      = ps

	-- Given original y, position and skyline tail, calculate wasted space.
	-- The rectangle is assumed to fit.
	skyWaste :: Word -> Position -> [Position] -> [(Position, Size)]
	skyWaste oy (Position px py) ps = skyWaste1 (px+rw) py px oy ps
	skyWaste1 _ _ _ _ [] = []
	skyWaste1 l c x0 y0 (Position x1 y1 : ps)
		| x1 >= l   = skyWaste2 x0 y0 (l-x0)  (c-y0) []
		| otherwise = skyWaste2 x0 y0 (x1-x0) (c-y0) (skyWaste1 l c x1 y1 ps)
	skyWaste2 x y w 0 cont = cont
	skyWaste2 x y 0 h cont = cont -- Should never happen.
	skyWaste2 x y w h cont = (Position x y, Size w h) : cont
	
	-- From the current skyline, generate solutions and select the minimum.
	skyMin :: PackM Position
	skyMin = skyMin1 (sizeHeight binSize) [] binSkyline
	skyMin1 _ _ [] = mempty
	skyMin1 y0 rps (p@(Position x1 y1) : ps)
		| y1 >= y0  = skyMin1 y1 (p:rps) ps
		| otherwise = skyMin2 x1 y1 rps ps <> skyMin1 y1 (p:rps) ps
	skyMin2 x y rps ps = fromMaybe mempty $ do
		let pos0 = Position x y
		nh <- skyFit pos0 ps
		let pos1 = pos0 { positionY = nh }
		let waste = skyWaste (positionY pos0) pos1 ps
		return $ PackM pos1 pos1 bin
			{ binSkyline    = skyGlue rps $ skyTail pos1 ps
			, binGuillotine = guilComb waste binGuillotine
			}
		

	skyline = fromPackM skyMin

	-- Given wasted space and guillotine, combine them to a new guillotine.
	guilComb :: [(Position, Size)] -> Map Position Size -> Map Position Size
	guilComb [] g0 = g0
	guilComb ((pos,sz):ns) g0 = guilComb1 pos sz $ foldr (\(p,s) -> M.insert p s) g0 ns
	guilComb1 pos sz g0 = fromMaybe (M.insert pos sz g0) $ do
		(posL,szL) <- M.lookupLE pos g0
		guard
			$  positionY pos == positionY posL
			&& sizeHeight sz == sizeHeight szL
			&& positionX posL + sizeWidth szL == positionX pos
		let nsz = Size (sizeWidth sz + sizeWidth szL) (sizeHeight sz)
		pure $ M.insert posL nsz g0

	-- Given placement position and free size, calculate the new guillotine.
	guilNew :: Position -> Size -> Map Position Size
	guilNew pos free = g3 where
		g1 = M.delete pos binGuillotine
		g2 = if rh < sizeHeight free
			then guilComb1
				(Position (positionX pos) (positionY pos + rh))
				(Size rw (sizeHeight free - rh))
				g1
			else g1
		g3 = if rw < sizeWidth free
			then M.insert
				(Position (positionX pos + rw) (positionY pos))
				(Size (sizeWidth free - rw) (sizeHeight free))
				g2
			else g2

	-- From the current guillotine, generate solutions and select the
	-- one with minimum waste.
	guilMin :: PackM Word
	guilMin = M.foldMapWithKey guilMin1 binGuillotine
	guilMin1 pos free
		| rw <= sizeWidth free && rh <= sizeHeight free
			= PackM (sizeWidth free * sizeHeight free - rw * rh) pos bin
				{ binGuillotine = guilNew pos free
				}
		| otherwise = mempty
	
	guillotine = fromPackM guilMin

-- | An array of 'Bin's.
newtype BinArray
	= BinArray
	{ binArrayBins :: Seq Bin
	}

-- | An empty 'BinArray' with the given number of layers and bin size.
emptyBinArray :: Word -> Size -> BinArray
emptyBinArray depth size = BinArray $ Q.replicate (fromIntegral depth) $ emptyBin size

-- | Allocate a rectangle from a 'BinArray'.
packArray
	:: Size     -- ^ The requested size of the rectangle.
	-> BinArray -- ^ The 'BinArray' from which to allocate.
	-> Maybe (Word, Position, BinArray) -- ^ The zero based 'Bin' index and the bottom left coordinate of the allocated rectangle and the new 'BinArray'.
packArray size BinArray{..} = foldr (<|>) Nothing $ fmap tryPack [0..length binArrayBins] where
	tryPack i = do
		let bin0 = binArrayBins `Q.index` i
		(pos, bin1) <- pack size bin0
		let arr1 = BinArray { binArrayBins = Q.update i bin1 binArrayBins }
		pure (fromIntegral i, pos, arr1)

