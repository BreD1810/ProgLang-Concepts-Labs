import System.IO
import Control.Exception
import Data.List
import Data.List.Split

multiZipL :: Eq a => [[a]] -> [[a]]
multiZipL [] = []
multiZipL xs = [y | (y:ys) <- xs] : multiZipL [ys | (y:ys) <- xs, ys /= []]

multiFileZipL :: () -> IO ()
multiFileZipL () = catch (multiFileZipL' ()) filehandler

multiFileZipL' :: () -> IO ()
multiFileZipL' () = do s <- readFile "lists.csv"
                       let ls = lines(s)
                       let sss = map (splitOn ",") ls
                       let iss = map (map read) sss :: [[Int]]
                       let mss = multiZipL iss
                       let oss = map (map show) mss
                       let css = intercalate "\n" (map (intercalate ",") oss)
                       writeFile "ziplists.csv" css

filehandler :: IOException -> IO ()
filehandler e = do let err = show (e :: IOException)
                   hPutStr stderr ("Warning: Couldn't open input/output file: " ++ err)
                   return ()

main :: IO ()
main = do
    multiFileZipL ()