import Network
import System.IO
import System.Environment (getArgs)
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
    handle <- connectTo "127.0.0.1" (PortNumber 8000)
    hSetBuffering handle LineBuffering
    input <- getLine
    hPutStr handle input
    contents <- hGetContents handle
    putStrLn (contents)
    hClose handle
    