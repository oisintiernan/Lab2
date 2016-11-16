import Network
import System.IO
import System.Environment (getArgs)
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
    echo

echo :: IO ()
echo = do
	
	h <- connectTo "0.0.0.0" (PortNumber 8000)
    --h <- connectTo "www.scss.tcd.ie" (PortNumber 80)
	hSetBuffering h LineBuffering--means that each character wont consume a whole packet to itself
	talk h 
	
    --hPutStr h ("GET /~ebarrett/lectures/cs4032/echo.php?message=" ++ input' ++ "\nHost: www.scss.tcd.ie  \r\n\r\n")
	

talk :: Handle -> IO ()
talk hdl = do
	putStrLn "What command do you want to send the server"
	input' <- getLine
	hPutStrLn hdl (input')
	contents <- hGetLine hdl
	putStrLn (contents)
	talk hdl