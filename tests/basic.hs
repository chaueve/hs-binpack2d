
import Data.Maybe

import Control.Monad

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Data.BinPack2D

instance QC.Arbitrary Size where
	arbitrary = do
		w <- QC.choose (1, 10)
		h <- QC.choose (1, 10)
		pure $ Size w h

newtype LargeSize = LargeSize Size
instance QC.Arbitrary LargeSize where
	arbitrary = do
		w <- QC.choose (20, 50)
		h <- QC.choose (40, 100)
		pure $ LargeSize $ Size w h
instance Show LargeSize where
	show (LargeSize sz) = show sz

main = defaultMain $ testGroup "Basic"
	[ testGroup "Randomized"
		[ QC.testProperty "All draws are inside bin" inFrameCheck
		, QC.testProperty "There are no overlaps" noOverlapCheck
		, QC.testProperty "Has acceptable fill ratio (may fail)" acceptableRatioCheck
		-- There is some problem with this.
		-- The bin should not be lossy even though it is fragmenting.
		--, QC.testProperty "Long and fine enough rectangle sequence fills the bin" fullCheck
		]
	]

inFrameCheck (LargeSize bsize) (QC.InfiniteList szs _) = loop (emptyBin bsize) (take 1000 szs) where
	loop bin0 [] = True
	loop bin0 (sz:szs) = case pack sz bin0 of
		Nothing -> loop bin0 szs
		Just (pos, bin1) -> inFrame pos sz && loop bin1 szs
	inFrame (Position x0 y0) (Size w0 h0)
		= (fromIntegral x0 :: Int) >= 0 && (fromIntegral y0 :: Int) >= 0
		&& x0 + w0 <= sizeWidth bsize && y0 + h0 <= sizeHeight bsize

noOverlapCheck (LargeSize bsize) (QC.InfiniteList szs _) = loop (emptyBin bsize) (take 1000 szs) [] where
	loop bin0 [] rcs = True
	loop bin0 (sz:szs) rcs = case pack sz bin0 of
		Nothing -> loop bin0 szs rcs
		Just (pos, bin1) -> check pos sz rcs && loop bin1 szs ((pos,sz) : rcs)
	check pos sz rcs = inner rcs where
		inner [] = True
		inner ((rpos, rsz): rcs) = separate pos sz rpos rsz && inner rcs
	separate (Position x0 y0) (Size w0 h0) (Position x1 y1) (Size w1 h1)
		= x0 + w0 <= x1 || x1 + w1 <= x0 || y0 + h0 <= y1 || y1 + h1 <= y0

acceptableRatioCheck :: LargeSize -> (QC.InfiniteList Size) -> Bool
acceptableRatioCheck (LargeSize bsize) (QC.InfiniteList szs _) = loop 0 (emptyBin bsize) (take 1000 szs) where
	area sz = sizeWidth sz * sizeHeight sz
	loop _ _ [] = False
	loop acc bin0 (sz:szs) = case pack sz bin0 of
		Nothing          -> loop acc bin0 szs
		Just (pos, bin1) -> loop1 (acc + area sz) bin1 szs
	loop1 acc bin szs = if fromIntegral acc >= enough then True else loop acc bin szs
	enough = 0.90 * fromIntegral (area bsize)

fullCheck (LargeSize bsize) (QC.InfiniteList szs _)
	= loop 0 (emptyBin bsize) (take 1000 szs ++ replicate (fromIntegral binArea) (Size 1 1)) where
		area sz = sizeWidth sz * sizeHeight sz
		binArea = area bsize
		loop _ _ [] = False
		loop acc bin0 (sz:szs) = case pack sz bin0 of
			Nothing          -> loop acc bin0 szs
			Just (pos, bin1) -> loop1 (acc + area sz) bin1 szs
		loop1 acc bin szs
			| acc > binArea  = error "Overdraw detected!"
			| acc == binArea = True
			| otherwise      = loop acc bin szs

