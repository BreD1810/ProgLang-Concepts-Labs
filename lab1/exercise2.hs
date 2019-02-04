zipl :: ([a], [a]) -> [[a]]
zipl ([], []) = []
zipl ([], (x:xs)) = [x] : (zipl ([], xs))
zipl ((x:xs), []) = [x] : (zipl (xs, []))
zipl ((x:xs), (y:ys)) = [x, y] : (zipl (xs, ys))

-- It would not be possible to write unzipl in this case,
-- as you would be unable to determine if the element came from
-- the left or the right list