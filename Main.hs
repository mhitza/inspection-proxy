module Main where


    import Control.Proxy.TCP
    import Control.Proxy
    import System.Console.CmdArgs.Explicit

    import Control.Monad (void, unless)
    import Data.Maybe (isNothing)
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

    proxyDetails :: [(String, t)] -> Maybe (t, t, t)
    proxyDetails as = readProxyDetails $ filter (\(a,_) -> a == "proxy-setup") as where
        readProxyDetails as' | length as' /= 3 = Nothing
                             | otherwise       = Just (snd $ as' !! 2, snd $ as' !! 1, snd $ head as')  -- arguments are parsed in reverse order


    printPass :: Proxy p => Bool -> () -> p () B.ByteString b' B.ByteString IO r
    printPass skip () = runIdentityP $ forever $ do
        readValue <- request ()
        unless skip $ lift $ B.putStrLn readValue
        void $ respond readValue


    main :: IO ()
    main = do
        args <- processArgs arguments 
        let connectionDetails = proxyDetails args
        let hasFlag flag      = (flag, "") `elem` args
        if hasFlag "help" || isNothing connectionDetails
            then print $ helpText [] HelpFormatDefault arguments
            else do
                let Just (bindport, host, port) = connectionDetails
                serve HostAny bindport $ \(bindSocket, _) -> 
                    connect host port $ \(serviceSocket, _) -> do
                        void $ Async.async $ runProxy $ socketReadS 4096 bindSocket >-> printPass (hasFlag "only-server") >-> socketWriteD serviceSocket 
                        runProxy $ socketReadS 4096 serviceSocket >-> printPass (hasFlag "only-client") >-> socketWriteD bindSocket 
                return ()
