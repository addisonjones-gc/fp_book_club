type FirstName = String
type LastName = String
type MiddleName = String
type FirstInitial = Char
type SecondInitial = Char
type LastInitial1 = Char
type LastInitial2 = Char

data Name = Name FirstName LastName
   | NameWithMiddle FirstName MiddleName LastName
   | FirstNameWithTwoInitials FirstName LastInitial1 LastInitial2
   | TwoInitialsWithLast FirstInitial SecondInitial LastName


instance Show Name where
    show (Name firstname lastname) = firstname ++ " " ++ lastname
    show (NameWithMiddle firstname middlename lastname) = firstname ++ " " ++ middlename ++ " " ++ lastname
    show (TwoInitialsWithLast firstinitial secondinitial lastname) = firstinitial:". " ++ secondinitial:". " ++ lastname
    show (FirstNameWithTwoInitials firstname lastinitial1 lastinitial2) = firstname ++ " " ++ lastinitial1:". " ++ lastinitial2:". "

newtype Author = Author Name 
instance Show Author where
    show (Author a) = show a

data Artist = Person Name | Band String 
instance Show Artist where
    show (Person a) = show a
    show (Band a) = show a

data Creator = AuthorCreator Author | ArtistCreator Artist 
instance Show Creator where
    show (AuthorCreator a) = show a
    show (ArtistCreator a) = show a

data Book = Book 
    { author     :: Creator
    , isbn       :: String
    , bookTitle  :: String
    , bookYear   :: Int
    , bookPrice  :: Double
    }

data VinylRecord = VinylRecord 
    { artist     :: Creator
    , recordTitle  :: String
    , recordYear   :: Int
    , recordPrice  :: Double
    }

data CollectableToy = CollectableToy 
    { name :: String 
    , description :: String
    , toyPrice :: Double
    }

data Pamphlet = Pamphlet 
    { pamphletTitle       :: String 
    , pamphletDescription :: String 
    , contact             :: String
    }


data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | ToyItem CollectableToy
               | PampletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book 
price (RecordItem record) = recordPrice record 
price (ToyItem toy) = toyPrice toy
price _ = 0

hpLovecraft :: Creator
hpLovecraft = AuthorCreator 
                (Author 
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

madeBy :: StoreItem -> String
madeBy (BookItem book) = mconcat [bookTitle book, ": Created by ", show (author book)] 
madeBy (RecordItem record) = recordTitle record ++ ": Created by " ++ show (artist record) 
madeBy (PampletItem pamphlet) = "Contact " ++ contact pamphlet ++ " for information"
madeBy _ = "Creator unknown"


-- Q16.2

-- data Shape = Circle | Square | Rectangle

data Circle = Circle {
      radius :: Double 
}

data Square = Square {
      sqWidth :: Double
} 

data Rectangle = Rectangle {
      recWidth  :: Double 
    , recHeight :: Double
} 

data Shape = CircleShape Circle | SquareShape Square | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter a = case a of
    CircleShape c -> 2 * pi * radius c
    SquareShape s -> 4 * sqWidth s
    RectangleShape r -> (2 * recWidth r) + (2 * recHeight r)

main :: IO()

main = do
    print hpLovecraft
    let necro = BookItem (Book hpLovecraft "123abc" "Necronomicon" 1923 12.99)
    print $ madeBy necro

    let p = PampletItem (Pamphlet "What to do" "Tourist shit" "jason@gocaribou.com")
    print $ madeBy p

    let sq = CircleShape (Circle 4.00)
    print (show (perimeter sq))
