module ListHelp where

transpose : [[a]] -> [[a]]
transpose xs = case xs of
                 [] :: _ -> []
                 _ -> (map head xs) :: transpose (map tail xs)

(!!) : [a] -> Int -> Maybe a
xs !! n = case (xs,n) of
            ([], _)       -> Nothing
            (x :: _, 0)   -> Just x
            (_ :: xs', n) -> xs' !! (n - 1)

groupsOf : Int -> [a] -> [[a]]
groupsOf n xs = case xs of
                  [] -> []
                  _  -> take n xs :: groupsOf n (drop n xs)
