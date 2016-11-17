import System.IO 
import Network.Socket
import Control.Concurrent --library which has functionality allowing multiple connections at a time
import Data.List.Split
import Control.Monad

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    putStrLn "hello"
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable
    bind sock (SockAddrInet 8080 (tupleToHostAddress (10, 62, 0, 232)))  
    listen sock 2                              -- set a max of 2 queued connections                          -- unimplemented
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    forkIO(runConn conn)           -- run our server's logic           -- repeat
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, sa) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    messaging  sock sa hdl
    
messaging :: Socket -> SockAddr -> Handle -> IO()
messaging  sock addr hdl = do
    putStrLn "looped/started successfully"
    contents <- hGetLine hdl
    putStrLn (contents)
    if (contents == "KILL_SERVICE")
        then cls sock hdl
        else if (contents == "HELO BASE_TEXT")
            then infoSplit addr hdl
            else hPutStrLn hdl "keep going"
    messaging sock addr hdl

cls:: Socket -> Handle -> IO ()
cls sock hdl = do
    hPutStrLn hdl "gluck"
    hClose hdl



infoSplit:: SockAddr -> Handle -> IO()
infoSplit sa hdl = do
    let address = (show sa)
    let a = splitOn ":" address
    hPutStr hdl ("HELO text\n IP:" ++ (sq (a !! 0)) ++ "\n" ++ " Port:" ++ (sq (a !! 1)) ++"\n" ++ " StudentID:12312629\n")--(show (a !! 1))

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
        | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
        | otherwise      = s
sq s                         = s

    