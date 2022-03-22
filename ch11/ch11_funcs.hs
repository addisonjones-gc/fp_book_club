halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Double -> String
printDouble d = show (d * 2)

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress = (\number -> 
                \street ->
                 \town -> (number, street, town))


-- Q11.1
-- Filter would be like filter :: (a -> a) -> [a] -> [a] because 
-- it's returning a subset of the list which would need to remain the same type

-- Q11.2
-- Can't write this since head has to return a value from the list rather than a list?
-- head :: [a] -> a
-- head [] = []
-- head (x:xs) = x

-- Q11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
                      where newInit = f init x

main :: IO()
main = do
    print (halve 4)

    print (printDouble 5.0)

    print (makeAddress 123 "Happy St" "Haskell town")