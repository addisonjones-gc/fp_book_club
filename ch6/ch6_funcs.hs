custRepeat e = (cycle [e])

subseq start end l = drop start (take end l)

main :: IO()
main = do
    let infa = custRepeat "a"
    let as = take 4 infa
    print as

    let b = subseq 0 5 "abcdefg"
    print b