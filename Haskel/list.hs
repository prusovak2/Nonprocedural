flatten ::[[a]] -> [a]
flatten [] = []
flatten [[a]] = [a]
flatten (x:xs) = append x (flatten xs) 

--append :: [a] ->[a]
append [] x = x
append (x:xs) y = x:(append xs y) 

dezip :: [(a,a)] -> ([a],[a])
dezip [] = ([],[]) 
dezip ((x,y):xs) = (x:fst rest,y:(snd rest))
    where rest = dezip xs
    
    
--rev::[a]->[a]    
--rev [] = []
--rev (x:xs) = (rev xs) ++ [x]

rev::[a]->[a] 
rev = foldl(\acc x -> x: acc) []