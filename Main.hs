{-# LANGUAGE DeriveDataTypeable #-}
module Main where


    import System.Environment (getArgs)
    import Control.Proxy.TCP
    import Control.Proxy
    import System.Console.CmdArgs

    import Control.Monad (void)
    import qualified Control.Concurrent.Async as Async
    import qualified Data.ByteString.Char8 as B


    readDispose :: Proxy p => () -> p () B.ByteString b' B.ByteString IO r
    readDispose () = runIdentityP $ forever $ do
        readValue <- request ()
        lift $ B.putStrLn readValue
        void $ respond readValue


    main :: IO ()
    main = do
        (host:port:bindport:_) <- getArgs
        serve HostAny bindport $ \(bindSocket, _) -> 
            connect host port $ \(serviceSocket, _) -> do
                void $ Async.async $ runProxy $ socketReadS 4096 bindSocket >-> readDispose >-> socketWriteD serviceSocket 
                runProxy $ socketReadS 4096 serviceSocket >-> readDispose >-> socketWriteD bindSocket 
        return ()
