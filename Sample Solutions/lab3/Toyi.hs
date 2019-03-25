import ToyTokens
import ToyGrammar
import ToyEval
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do putStrLn ("Toy Interactive Mode - enter an expression : ")
           sourceText <- getLine
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg))
           let result = evalLoop (parsedProg)
           putStrLn ("Evaluates to " ++ (unparse result) ++ "\n")
           main'

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               putStrLn("----------------")
               hPutStrLn stderr err
               putStrLn("----------------")
               main


