multiZipL :: Eq a => [[a]] -> [[a]]
multiZipL [] = []
multiZipL xs = [y | (y:ys) <- xs] : multiZipL [ys | (y:ys) <- xs, ys /= []]