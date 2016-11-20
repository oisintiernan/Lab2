import Data.List.Split
import System.IO
import System.Exit
import System.Environment
import Network.Socket
import Network.BSD
import Control.Concurrent
import Control.Monad

myPool = 10

main :: IO ()
main = do
	args <- getArgs
	let port = read $ head args :: Int
	start port

start:: Int -> IO ()
start port = do
	sock <- socket AF_INET Stream 0    -- create socket    
	setSocketOption sock ReuseAddr 1   -- make socket immediately reusable
	bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (10,62, 0, 232)))  
	listen sock 3 
	(input,output) <- threadPoolIO myPool runConn                             -- set a max of 2 queued connections                          -- unimplemented
	mainLoop sock input port

mainLoop :: Socket -> Chan (Int,(Socket, SockAddr))-> Int -> IO ()
mainLoop sock input port = do
    conn <- accept sock     -- accept a connection and handle it
    writeChan input (port,conn)
    --forkIO(runConn conn)           -- run our server's logic           -- repeat
    mainLoop sock input port

runConn :: (Int,(Socket, SockAddr)) -> IO ()
runConn (port,(sock, sa)) = do
	t <- myThreadId
	putStrLn(show t)
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl  $ BlockBuffering (Nothing)
	messaging  sock sa hdl
    
messaging :: Socket -> SockAddr -> Handle -> IO()
messaging  sock addr hdl = do
    putStrLn "looped/started successfully"
    contents <- hGetLine hdl
    let msg = words contents
    putStrLn (contents)
    if (contents == "KILL_SERVICE\n")
        then hClose hdl
        else if ( (msg !! 0) == "HELO")
            then infoSplit addr hdl
            else putStr "keep going\n"
    messaging sock addr hdl

cls:: Socket -> Handle -> IO ()
cls sock hdl = do
    hPutStr hdl "gluck"
    hFlush hdl
    hClose hdl



infoSplit:: SockAddr -> Handle -> IO()
infoSplit sa hdl = do
    let address = (show sa)
    let a = splitOn ":" address
    hPutStr hdl ("HELO text\n IP:" ++ (sq (a !! 0)) ++ "\n" ++ " Port:" ++ (sq (a !! 1)) ++"\n" ++ " StudentID:12312629")--(show (a !! 1))
    hFlush hdl

sq :: String -> String
sq s@[c]                     = s
sq ('"':s)  | last s == '"'  = init s
        | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
        | otherwise      = s
sq s                         = s

   
threadPoolIO :: Int -> (a -> IO b) -> IO (Chan a, Chan b)
threadPoolIO nr mutator = do
    input <- newChan
    output <- newChan
    forM_ [1..nr] $
        \_ -> forkIO (forever $ do
            i <- readChan input
            o <- mutator i
            writeChan output o)
    return (input, output)

