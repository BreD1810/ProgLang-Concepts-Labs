import Tokens
import Grammar
import System.IO
import System.Environment


main :: IO ()
main = do
    (fileName : _) <- getArgs
    code <- readFile fileName
    let program = parseCalc (alexScanTokens code)
    putStrLn (show program)

