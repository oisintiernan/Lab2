import Network
import System.IO
import System.Environment (getArgs)
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
    handle <- connectTo "127.0.0.1" (PortNumber 8000)
    hSetBuffering handle LineBuffering
    echo handle
    hClose handle

echo :: Handle -> IO ()
echo handle = do
    input <- getLine
    hPutStrLn handle input
    contents <- hGetContents handle
    putStrLn (contents)
    if (contents == "q")
        then return()
        else echo handle
    