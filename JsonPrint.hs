module Main where

    import Data.JSON
    import Data.Aeson.Types
    import qualified Data.HashMap.Strict as HM
    import qualified Data.Vector as V
    import qualified Data.ByteString as B

    
    structuralJsonTransform (Object o) = concatMap (structuralJsonTransform . snd) $ HM.toList o
    structuralJsonTransform (Array a)  = concatMap structuralJsonTransform $ V.toList a
    structuralJsonTransform v          = [v]


    main :: IO ()
    main = B.getContents >>= print . fmap structuralJsonTransform . Data.JSON.parseJSON
