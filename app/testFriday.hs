{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative

import Vision.Image.Conversion
import Vision.Image
import qualified Vision.Image.Class as IC
import Vision.Image.Storage.DevIL
import System.Environment (getArgs)

import Control.Monad (forM_)

zipWithI :: (Image src1, Image src2, Image res, Convertible (Delayed (ImagePixel res)) res) =>
                (ImagePixel src1 -> ImagePixel src2 -> ImagePixel res) -> src1 -> src2 -> res
zipWithI f src1 src2 = convert $ Delayed (shape src1) (f <$> delayedFun (delay src1) <*> delayedFun (delay src2))


sumSq :: Num a => a -> a -> a
sumSq x y = x*x + y*y


quadSum :: Floating a => a -> a -> a
quadSum x y = sqrt (sumSq x y)


cutOff :: Ord a => a -> a -> a
cutOff mx x = if x < mx then x else mx


toGreen, toRed, toBlue, toAlpha :: (Convertible src RGBA) => src -> GreyDelayed
toGreen src = IC.map (GreyPixel . rgbaGreen) (convert src :: RGBA)
toRed src = IC.map (GreyPixel . rgbaRed) (convert src :: RGBA)
toBlue src = IC.map (GreyPixel . rgbaBlue) (convert src :: RGBA)
toAlpha src = IC.map (GreyPixel . rgbaAlpha) (convert src :: RGBA)

main :: IO ()
main = do
        fin : fout : _ <- getArgs
        i <- load Autodetect fin :: IO (Either StorageError RGBA)
        case i of
            Left err -> error (show err)
            Right img ->
                forM_ [(toRed, "Red"), (toBlue, "Blue"), (toGreen, "Green"), (toAlpha, "Alpha")] $
                    \(f, n) -> do
                            let img' = f img
                            let imgX = scharr DerivativeX img' :: GreyDelayed
                            let imgY = scharr DerivativeY img' :: GreyDelayed

                            save Autodetect (fout ++ n ++ "X.jpg") $ compute imgX
                            save Autodetect (fout ++ n ++ "Y.jpg") $ compute imgY
                            save Autodetect (fout ++ n ++ "XY.jpg") (zipWithI (\x y -> cutOff 255 (x + y)) imgX imgY :: Grey)
