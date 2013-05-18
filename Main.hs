module Main where


    import System.Environment (getArgs)
    import Control.Proxy.TCP
    import Control.Concurrent
    import Control.Monad 
    import Control.Proxy

    import Control.Concurrent.MVar
    import qualified Data.ByteString as B


    readDispose () = runIdentityP $ forever $ do
        readValue <- request ()
        lift $ B.putStrLn readValue
        respond readValue
        return ()


    main :: IO ()
    main = do
        m' <- newEmptyMVar
        (host:port:bindport:_) <- getArgs
        serve HostAny bindport $ \(bindSocket, _) -> do
            connect host port $ \(serviceSocket, _) -> do
                c1 <- newEmptyMVar
                c2 <- newEmptyMVar
                forkIO $ do
                    runProxy $ socketReadS 4096 bindSocket >-> readDispose >-> socketWriteD serviceSocket 
                    putMVar c1 True
                forkIO $ do
                    runProxy $ socketReadS 4096 serviceSocket >-> readDispose >-> socketWriteD bindSocket 
                    putMVar c2 True
                takeMVar c1
                return ()
        takeMVar m'
        return ()
