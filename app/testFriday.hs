{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative

import Vision.Image.Conversion
import Vision.Image
import Vision.Image.Storage.DevIL
import System.Environment (getArgs)

zipWithI :: (Image src1, Image src2, Image res, Convertible (Delayed (ImagePixel res)) res) =>
                (ImagePixel src1 -> ImagePixel src2 -> ImagePixel res) -> src1 -> src2 -> res
zipWithI f src1 src2 = convert $ Delayed (shape src1) (f <$> delayedFun (delay src1) <*> delayedFun (delay src2))


sumSq :: Num a => a -> a -> a
sumSq x y = x*x + y*y


quadSum :: Floating a => a -> a -> a
quadSum x y = sqrt (sumSq x y)


cutOff :: Ord a => a -> a -> a
cutOff mx x = if x < mx then x else mx


main :: IO ()
main = do
        fin : foutX : foutY : foutXY : _ <- getArgs
        img <- load Autodetect fin :: IO (Either StorageError Grey)
        let imgX = scharr DerivativeX <$> img :: Either StorageError Grey
        let imgY = scharr DerivativeY <$> img :: Either StorageError Grey

        case imgX of
            Left err -> error (show err)
            Right ix -> save Autodetect foutX (convert ix :: Grey)
            
            
        case imgY of
            Left err -> error (show err)
            Right iy -> save Autodetect foutY (convert iy :: Grey)
            
        case liftA2 (,) imgX imgY of
            Right (ix, iy) -> save Autodetect foutXY (zipWithI (\x y -> cutOff 255 (x + y)) ix iy :: Grey)
            _ -> return Nothing

        return ()
