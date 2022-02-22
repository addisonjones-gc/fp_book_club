inc x = x + 1
double x = x * 2
square x = x ** 2

evenMinus2 n = if isEven
               then n - 2
               else 3 * n + 1
    where isEven = even n

main :: IO()
main = do
    let z = inc 5
    let x = double 5
    let y = square 5

    let iet = evenMinus2 6
    let ief = evenMinus2 5
    print iet
    print ief

    -- print z
    -- print x
    -- print y

