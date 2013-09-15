module Main where


    import Control.Monad (void, unless, when, forever)
    import Data.Maybe (isNothing)
    import Control.Concurrent.Async (async)
    import qualified Data.ByteString.Char8 as B
    import Pipes
    import Pipes.Network.TCP
    import System.Console.CmdArgs.Explicit


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


    printPass :: Bool -> Pipe B.ByteString B.ByteString IO r
    printPass skip = forever $ do
        readValue <- await
        unless skip $ lift $ B.putStrLn readValue
        yield readValue


    main :: IO ()
    main = do
        args <- processArgs arguments 
        let connectionDetails = proxyDetails args
        let hasFlag flag      = (flag, "") `elem` args
        when (hasFlag "version") $ putStrLn "inspection-proxy version 0.1.0.2"
        if hasFlag "help" || isNothing connectionDetails
            then print $ helpText [] HelpFormatDefault arguments
            else do
                let Just (bindport, host, port) = connectionDetails
                serve HostAny bindport $ \(bindSocket, _) -> 
                    connect host port $ \(serviceSocket, _) -> do
                        void $ async $ runEffect $ fromSocket bindSocket 4096 >-> printPass (hasFlag "only-server") >-> toSocket serviceSocket 
                        runEffect $ fromSocket serviceSocket 4096 >-> printPass (hasFlag "only-client") >-> toSocket bindSocket 
                return ()
