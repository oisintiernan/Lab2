import System.IO 
import Network.Socket
import Control.Concurrent --library which has functionality allowing multiple connections at a time
import Data.List.Split

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable
    bind sock (SockAddrInet 8080 0x8E00E3A0)   -- listen on TCP port 8000 address 127.0.0.10x8E00E3A0
    putStrLn "listening"
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock                              -- unimplemented
-- in Main.hs
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    putStrLn "In Main Loop"
    forkIO(runConn conn)           -- run our server's logic
    mainLoop sock           -- repeat
 
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
        else if (contents == "HELO text")
            then infoSplit addr hdl
            else hPutStrLn hdl "keep going"
    messaging sock addr hdl

cls:: Socket -> Handle -> IO ()
cls sock hdl = do
    hPutStrLn hdl "gluck"
    close sock



infoSplit:: SockAddr -> Handle -> IO()
infoSplit sa hdl = do
    let address = (show sa)
    let a = splitOn ":" address
    hPutStrLn hdl ("HELO text IP:" ++ (sq (a !! 0)) ++ " Port:" ++ (sq (a !! 1)) ++ " StudentID:12312629\n")--(show (a !! 1))

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
        | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
        | otherwise      = s
sq s                         = s

    --if (contents == "KILL_SERVICE")
    --    then hPutStrLn hdl "terminating service"
    --    else if (contents == "HELO text")
    --        then hPutStrLn hdl "HELO text\nIP:[ip address]\nPort:[port number]\nStudentID:[your student ID]\n"
    --       else messaging addr hdl
    

--contents <- hGetLine hdl
    --putStrLn (contents)
    --if (contents == "KILL_SERVICE")
    --    then hPutStrLn hdl "terminating service"
    --    else if (contents == "HELO text")
    --        then hPutStrLn hdl "HELO text\nIP:[ip address]\nPort:[port number]\nStudentID:[your student ID]\n"
    --        else hPutStrLn hdl "wha???"
    --hClose hdl
    --hdl <- socketToHandle sock ReadWriteMode
    
    --putStrLn "In RunConn"
    --hSetBuffering hdl NoBuffering
    --hPutStrLn hdl "Hello!"
--hClose hdl