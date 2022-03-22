type FirstName = String
type MiddleName = String
type LastName = String
newtype Age = Age Int deriving (PatientAttrib)
type Height = Int
type Weight = Int
type PatientName = (String,String)

data Name = Name FirstName LastName |
            NameWithMiddle FirstName MiddleName LastName

class PatientAttrib a where
    attribPrintLn :: a -> String

instance Age Int where
    attribPrintLn Int = "Age: " ++ show Int

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

firstName :: Name -> String
firstName (Name f l) = f
firstName (NameWithMiddle f m l) = f

lastName :: Name -> String
lastName (Name f l) = l
lastName (NameWithMiddle f m l) = l

data Sex = Male | Female deriving (Show)
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O deriving (Show)
data BloodType = BloodType ABOType RhType
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = show abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo  _ (BloodType AB _)= True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

data Patient = Patient { name :: Name 
                       , sex :: Sex 
                       , age :: Age 
                       , height :: Height 
                       , weight :: Weight 
                       , bloodType :: BloodType 
                       }

patCanDonateTo :: Patient -> Patient -> Bool
patCanDonateTo pat1 pat2 = canDonateTo (bloodType pat1) (bloodType pat2)

starLine :: String
starLine = foldl (++) "" (take 15 (cycle ["*"]))

showNameSummary :: Name -> String
showNameSummary (Name f l) = "Patient Name: " ++ l ++ ", " ++ f
showNameSummary (NameWithMiddle f m l) = "Patient Name: " ++ l ++ ", " ++ f

showPatName :: Patient -> String
showPatName pat = showNameSummary (name pat)

showPatSex :: Patient -> String
showPatSex pat = "Sex: " ++ show (sex pat)

showPatHeight :: Patient -> String
showPatHeight pat = "Height :" ++ show (height pat)

showPatAge :: Patient -> String
showPatAge pat = "Age: " ++ show (age pat)

showPatWeight :: Patient -> String
showPatWeight pat = "Weight: " ++ show (weight pat)

showPatBloodType :: Patient -> String
showPatBloodType pat = "Blood Type: " ++ showBloodType (bloodType pat)

patientSummary :: Patient -> String
patientSummary pat = starLine ++ "\n"
                     ++ showPatName pat ++ "\n"
                     ++ showPatSex pat ++ "\n"
                     ++ showPatAge pat ++ "\n"
                     ++ showPatHeight pat ++ "\n"
                     ++ showPatWeight pat ++ "\n"
                     ++ showPatBloodType pat ++ "\n"
                     ++ starLine

main :: IO()
main = do

    -- let jes = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 25 70 140 (BloodType AB Pos)

    let age1 = Age 41
    attribPrintLn age1
    -- putStrLn (patientSummary jes)

    -- let donate = canDonateTo (BloodType AB Pos) (bloodType jes)
    
    -- putStrLn (show donate)