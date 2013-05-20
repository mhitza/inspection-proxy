module Main where


    import System.Environment (getArgs)
    import Control.Proxy.TCP
    import Control.Proxy

    import qualified Control.Concurrent.Async as Async
    import qualified Data.ByteString.Char8 as B


    readDispose :: Proxy p => () -> p () B.ByteString b' B.ByteString IO r
    readDispose () = runIdentityP $ forever $ do
        readValue <- request ()
        lift $ B.putStrLn readValue
        respond readValue >> return ()


    async_ :: IO a -> IO ()
    async_ f = Async.async f >> return ()


    main :: IO ()
    main = do
        (host:port:bindport:_) <- getArgs
        serve HostAny bindport $ \(bindSocket, _) -> 
            connect host port $ \(serviceSocket, _) -> do
                async_ $ runProxy $ socketReadS 4096 bindSocket >-> readDispose >-> socketWriteD serviceSocket 
                runProxy $ socketReadS 4096 serviceSocket >-> readDispose >-> socketWriteD bindSocket 
        return ()
