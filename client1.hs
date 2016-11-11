import Network
import System.IO
import System.Environment (getArgs)
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
    echo

echo :: IO ()
echo = do
	putStrLn "What command do you want to send the server"
	input' <- getLine
	h <- connectTo "10.62.0.231" (PortNumber 8000)
    --h <- connectTo "www.scss.tcd.ie" (PortNumber 80)
	hSetBuffering h LineBuffering--means that each character wont consume a whole packet to itself
	hPutStrLn h (input')
    --hPutStr h ("GET /~ebarrett/lectures/cs4032/echo.php?message=" ++ input' ++ "\nHost: www.scss.tcd.ie  \r\n\r\n")
	contents <- hGetContents h
	putStr (contents)
	hClose h
	if (input' == "q")
	    then return()
	    else echo
    