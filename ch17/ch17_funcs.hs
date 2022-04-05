data Color = Red |
             Yellow |
             Blue |
             Green | 
             Purple | 
             Orange |
             Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
             | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
             | otherwise = Brown
            

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
 where totalProbs = sum probs
       normalizedProbs = map (/totalProbs) probs

showPair :: String -> Double -> String 
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
      where pairs = zipWith showPair events probs

main :: IO()
main = do
    let pt = createPTable ["heads", "tails"] [0.5, 0.5]
    print pt

-- cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
-- cartCombine func l1 l2 = zipWith func newL1 cycledL2
-- where nToAdd = length l2
--       repeatedL1 = map (take nToAdd . repeat) l1
--       newL1 = mconcat repeatedL1
--       cycledL2 = cycle l2