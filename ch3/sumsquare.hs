main :: IO()
sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum ) (x^2 + y^2) ((x+y)^2)

doubleDouble x = (\dubs -> dubs * 2) (x*2)

main = do
    let a = sumSquareOrSquareSum 2 4
    print a

    let b = doubleDouble 2
    print b