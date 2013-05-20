module Main where


    import System.Environment (getArgs)
    import Control.Proxy.TCP
    import Control.Proxy
    import System.Console.CmdArgs.Explicit

    import Control.Monad (void)
    import qualified Control.Concurrent.Async as Async
    import qualified Data.ByteString.Char8 as B

    arguments :: Mode [(String,String)]
    arguments = mode "inspection-proxy" [] "" (flagArg (upd "proxy-setup") "proxy-port server port")
        [ flagNone ["only-server"] (("only-server",""):) "print only server response activity"
        , flagNone ["only-client"] (("only-client",""):) "print only client request activity"
        , flagHelpSimple (("help",""):)
        , flagVersion (("version",""):)
        ]
        where upd msg x v = Right $ (msg,x):v


    readDispose :: Proxy p => () -> p () B.ByteString b' B.ByteString IO r
    readDispose () = runIdentityP $ forever $ do
        readValue <- request ()
        lift $ B.putStrLn readValue
        void $ respond readValue


    main :: IO ()
    main = do
        args <- processArgs arguments 
        if elem ("help","") args
            then print $ helpText [] HelpFormatDefault arguments
            else do
                (host:port:bindport:_) <- getArgs
                serve HostAny bindport $ \(bindSocket, _) -> 
                    connect host port $ \(serviceSocket, _) -> do
                        void $ Async.async $ runProxy $ socketReadS 4096 bindSocket >-> readDispose >-> socketWriteD serviceSocket 
                        runProxy $ socketReadS 4096 serviceSocket >-> readDispose >-> socketWriteD bindSocket 
                return ()
