module Main where


    import System.Environment (getArgs)
    import Control.Proxy.TCP
    import Control.Concurrent
    import Control.Monad 
    import Control.Proxy


    readDispose () = runIdentityP $ forever $ do
        readValue <- request ()
        respond readValue
        return ()

    main :: IO ()
    main = do
        (host:port:bindport:_) <- getArgs
        serve HostAny bindport $ \(bindSocket, _) -> do
            connect host port $ \(serviceSocket, _) -> do
                forkIO $ do runProxy $ socketReadS 100 bindSocket >-> readDispose >-> socketWriteD serviceSocket 
                forkIO $ do runProxy $ socketReadS 100 serviceSocket >-> readDispose >-> socketWriteD bindSocket 
                return ()
                    
