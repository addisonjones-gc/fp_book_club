-- import Text.Printf
cup flOz = \message -> message flOz
getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
        where flOz = getOz aCup
              ozDiff = flOz - ozDrank

main :: IO()
main = do
    let coffeeCup = cup 12
    let ozs = getOz coffeeCup
    print (getOz coffeeCup)

    let afterASip = drink coffeeCup 1
    print (getOz afterASip)
    let afterABigGulp = drink coffeeCup 3
    print (getOz afterABigGulp )