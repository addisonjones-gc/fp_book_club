data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Eq, Ord)
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum,Eq, Ord)

-- Can you change the Enum to an Int type?

class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)

main :: IO ()
main = do 
    print S1

    print (S1 == S2)
    print (S4 < S6)

    print (roll (Die FiveSidedDie))