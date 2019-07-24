
import Data.BinPack2D

import Control.Applicative
import Data.Maybe
import Text.Read

main = loop0 where
	loop0 :: IO ()
	loop0 = loop1 $ emptyBin (Size 10 10)
	loop1 :: Bin -> IO ()
	loop1 bin = getLine >>= \line -> fromMaybe (loopE bin) $ (loopR line <|>) $ do
		[sx, sy] <- pure $ words line
		x <- readMaybe sx
		y <- readMaybe sy
		pure $ loop2 bin x y
	loopR :: String -> Maybe (IO ())
	loopR "reset" = pure $ loop0
	loopR _       = Nothing
	loopE bin = do
		putStrLn "Parse error. Try again."
		loop1 bin
	loop2 bin0 x y = case pack (Size x y) bin0 of
		Nothing -> do
			putStrLn "Does not fit."
			loop1 bin0
		Just (pos, bin1) -> do
			putStrLn $ show pos
			putStrLn $ show bin1
			loop1 bin1

