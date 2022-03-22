import Data.Typeable

class Describable a where
    describe :: a -> String



main :: IO()
main = do
    let a = ["hey", "you", "guys"]
    -- print (show (typeOf a))
    putStrLn (show (typeOf a))
    putStrLn (show (typeOf Int))