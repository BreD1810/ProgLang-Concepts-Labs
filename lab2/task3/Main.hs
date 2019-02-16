import TokensMDL
import GrammarMDL
import System.IO
import System.Environment
import Control.Exception

main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do
    (fileName : _) <- getArgs
    code <- readFile fileName
    let program = parseCalc (alexScanTokens code)
    putStrLn (show program)

noParse :: ErrorCall -> IO ()
noParse e = do let err = show e
               hPutStr stderr err
               return ()