recurDrop 0 inputList = inputList
recurDrop n (x:xs) = recurDrop (n-1) xs
-- recurDrop n (x:xs) = xs

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myCollatz 1 = 1
myCollatz n = if even n
              then 1 + myCollatz (n `div` 2)
              else 1 + myCollatz (n*3 + 1)


myReverse [] = []
-- explain this v
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]
    

main :: IO()
main = do 
    -- let d = recurDrop 2 [1, 2, 3, 4]
    -- print d

    -- let l = myLength [1, 2, 3, 4]
    -- let z = myLength ['a'..'z']
    -- print l
    -- print z

    -- let c = myCollatz 9
    -- print c

    -- let r = myReverse [1, 2, 3]
    let r = myReverse [1]
    print r