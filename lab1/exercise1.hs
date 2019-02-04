zipl :: ([a], [a]) -> [[a]]
zipl ([], _) = []
zipl (_, []) = []
zipl ((x:xs), (y:ys)) = [x, y] : (zipl (xs, ys))

unzipl :: [[a]] -> ([a], [a])
unzipl [] = ([],[])
unzipl ([x,y]:zs) = let (xs,ys) = unzipl zs in (x:xs,y:ys)