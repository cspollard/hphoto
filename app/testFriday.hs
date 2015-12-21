module Main where

import Vision.Image.Conversion
import Vision.Image
import Vision.Image.Storage.DevIL
import System.Environment (getArgs)

main :: IO ()
main = do
        fin : fout : _ <- getArgs
        img <- load Autodetect fin :: IO (Either StorageError Grey)
        case img of
            Left err -> error (show err)
            Right i -> save Autodetect fout i

        return ()
