module Main where

    import Data.JSON

    import Data.ByteString
    import Prelude hiding (getContents)


    main :: IO ()
    main = getContents >>= print . parseJSON
