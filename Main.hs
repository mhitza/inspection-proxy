module Main where


    import System.Environment (getArgs)
    import Control.Proxy.TCP
    import Control.Concurrent
    import Control.Monad 
    import Control.Proxy

    import qualified Control.Concurrent.Async as Async
    import qualified Data.ByteString.Char8 as B


    readDispose () = runIdentityP $ forever $ do
        readValue <- request ()
        lift $ B.putStrLn readValue
        respond readValue
        return ()


    main :: IO ()
    main = do
        (host:port:bindport:_) <- getArgs
        serve HostAny bindport $ \(bindSocket, _) -> do
            connect host port $ \(serviceSocket, _) -> do
                a1 <- Async.async $ do runProxy $ socketReadS 4096 bindSocket >-> readDispose >-> socketWriteD serviceSocket 
                runProxy $ socketReadS 4096 serviceSocket >-> readDispose >-> socketWriteD bindSocket 
                Async.wait a1
        return ()
