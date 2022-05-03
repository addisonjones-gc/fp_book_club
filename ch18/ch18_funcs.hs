import qualified Data.Map as Map

data Box a = Box a deriving Show

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box (f a)

data Triple a = Triple a a a deriving Show

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z) 

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 1.2 3.2 4.8

data Organ = Heart | Lung | Brain | Kidney | Spleen deriving (Eq, Show, Ord)

organs :: [Organ]
organs = [Heart,Brain,Lung,Lung,Spleen,Kidney,Kidney]

distinctOrgans :: [Organ]
distinctOrgans = [Heart,Brain,Lung,Spleen,Kidney]

distinctOrganCounts :: [Int]
distinctOrganCounts = [1,1,2,1,2]

ids :: [Int]
ids = [2,7,13,14,21,24,72]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organCounts :: [(Organ,Int)]
organCounts = zip distinctOrgans distinctOrganCounts

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organCounts


main :: IO()
main = do
    print $ Triple 1 2 3
    print $ Map.lookup 7 organCatalog

    print $ Map.lookup Heart organInventory
    print $ Map.lookup Lung organInventory

    print $ boxMap (*2) (Box 2)

    print $ tripleMap (*3) (Triple 1 2 4)
