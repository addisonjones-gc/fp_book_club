data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
data BB = A | B deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize


rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)


fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN alphaSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

main :: IO ()
main = do
    print (rotN 4 L1)
    print (rotN 4 L2)
    print (rotN 4 L3)

    -- How does Haskell know that 'A' is part of the english alphabet?
    -- print(rotChar 'A')
    print(fromEnum 'A')
    print(rotChar '~')

    print $ fourLetterEncoder [L1, L2, L3, L4, L1, L2, L3, L4]

    -- Why use the ' for a helper function? Could it easily be given an arbitrary name?

    print (fromEnum (maxBound :: FourLetterAlphabet) + 1)