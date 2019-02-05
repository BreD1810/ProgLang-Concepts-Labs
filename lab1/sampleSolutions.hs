-- COMP2212
-- Exercise Sheet 1
-- Sample Solutions 
-- Dr Julian Rathke. Feb 2018

import Data.List
import Data.List.Split
import Control.Exception
import System.IO

-- Task 1
zipL :: ([a],[a]) -> [[a]]
zipL ([],[]) = []
zipL ((x:xs),(y:ys)) = [x,y]:zipL(xs,ys)

unzipL :: [[a]] -> ([a],[a])
unzipL [] = ([],[])
unzipL ([x,y]:zs) = (x:xs,y:ys)
      where (xs,ys) = unzipL zs

-- Task 2

zipLL :: ([a],[a]) -> [[a]]
zipLL ([],[]) = []
zipLL ((x:xs),[]) = [x] : zipLL(xs,[])
zipLL ([],(y:ys)) = [y] : zipLL([],ys)
zipLL ((x:xs),(y:ys)) = [x,y]:zipL(xs,ys)

-- Cannot reasonably write unzipLL as a singleton list in the input
-- could have come from either the first or second input. 
-- Could arbitrarily pick one of these but this would not make unzipLL 
-- an inverse function of zipLL

-- Task 3

-- This uses list comprehension in an interesting way 
-- When the pattern doesn't match (i.e. in (h:_) <- xss  where there is no head/tail element) then 
-- then the entry in xss is skipped over

multiZipL :: [[a]] -> [[a]]
multiZipL [] = []
multiZipL ([]   : xss) = multiZipL xss
multiZipL ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : multiZipL (xs : [ t | (_:t) <- xss])

-- Task 4 

-- Doing the read/show conversion is not necessary for performing multiZipL but 
-- it does tidy up the input and check that the input parses to integers
fileMZip :: () -> IO ()
fileMZip () = catch (fileMZip' ()) filehandler 

fileMZip' () = do s <- readFile "lists.csv"
                   let ls = lines(s)
                   let sss = map (splitOn ",") ls
                   let iss = map (map read) sss :: [[Int]]
                   let mss = multiZipL iss
                   let oss = map (map show) mss
                   let css = intercalate "\n" (map (intercalate ",") oss) 
                   writeFile "ziplists.csv" css

filehandler :: IOException -> IO ()  
filehandler e = do let err = show (e :: IOException)
                   hPutStr stderr ("Warning: Couldn't open input/output file : " ++ err)
                   return ()
