import System.IO 
import Network.Socket

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 8000 0x0100007f)   -- listen on TCP port 8000 address 127.0.0.1
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock                              -- unimplemented
-- in Main.hs
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    putStrLn "In Main Loop"
    runConn conn            -- run our server's logic
    mainLoop sock           -- repeat
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    contents <- hGetContents hdl
    putStrLn (contents)
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hello!"
    hClose hdl