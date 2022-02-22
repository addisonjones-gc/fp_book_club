import Data.Char
fToAll func val [] = []
fToAll func val (x:xs) = (x `func` val):fToAll func val xs

fMapToAll func val l = map (`func` val) l

myProduct xs = foldl (*) 1 xs

myElem :: Eq a => a -> [a] -> Bool
myElem goal xs = if (length filteredXs) > 0
                 then True
                 else False
         where filteredXs = filter (\x -> x == goal) xs


-- isPalindrome inputString = cleanedString == reversedCleaned
--                 where 
--                     cleanedString = filter (\x -> (x /= ' ')) (map toLower inputString)
--                     reversedCleaned = reverse cleanedString

isPalindrome inputString = let cleanedString = filter (\x -> (x /= ' ')) (map toLower inputString)
                               reversedCleaned = reverse cleanedString
                            in 
                             cleanedString == reversedCleaned

harmonic n = foldl (+) 0 (map (1/)[1..n]) 

main :: IO()
main = do
    print "fToAll +"
    let a = fToAll (+) 1 [1, 2]
    print a
    print "fToAll ^"
    let b = fToAll (^) 2 [2, 3]
    print b

    print "fMapToAll ^"
    let fm = fMapToAll (^) 2 [2, 3]
    print fm 
    print "myProduct"
    let mp = myProduct [2, 3]
    print mp

    print "Test elem"
    print (elem 1 [0, 1, 2])

    print "myElem" 
    let me = myElem 0 [0, 1, 2] 
    print me
    let men = myElem 3 [0, 1, 2] 
    print men
    print (myElem 1 [0, 1, 2])

    print "isPalindrome"
    let p = isPalindrome "A man a plan a canal Panama"
    print p

    print "harmonic"
    let h = harmonic 4
    print h