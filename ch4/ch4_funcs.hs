ifEven myFunction x = if even x
                      then myFunction x
                      else x

cubeEven ifEven x = ifEven (\x -> x^3) x

main :: IO()
main = do
    let a = cubeEven 2
    print a