newtype Name = Name String deriving (Show)
newtype Age = Age Int deriving (Show)
newtype Weight = Weight Int deriving (Show)
newtype Height = Height Int deriving (Show)

data Sex = Male | Female deriving (Show)

class PatientAttrib a where
    attribPrintLn :: a -> String

instance PatientAttrib Age where
    attribPrintLn (Age a) = "Age: " ++ show a ++ "\n"

instance PatientAttrib Name where
    attribPrintLn (Name a) = "Name: " ++ a ++ "\n"

instance PatientAttrib Weight where
    attribPrintLn (Weight a) = "Weight: " ++ show a ++ "\n"

instance PatientAttrib Height where
    attribPrintLn (Height a) = "Height: " ++ show a ++ "\n"

instance PatientAttrib Sex where
    attribPrintLn a = "Sex: " ++ show a ++ "\n"

data Patient = Patient { name :: Name 
                       , sex :: Sex 
                       , age :: Age 
                       , height :: Height 
                       , weight :: Weight 
                    --    , bloodType :: BloodType 
                       }

starLine :: String
starLine = foldl (++) "" (take 15 (cycle ["*"]))

patientSummary :: Patient -> String
patientSummary pat = starLine ++ "\n"
                     ++ attribPrintLn (name pat)
                     ++ attribPrintLn (sex pat)
                     ++ attribPrintLn (age pat)
                     ++ attribPrintLn (height pat)
                     ++ attribPrintLn (weight pat)
                     ++ starLine


main = do

    let les = Patient (Name "Addison Jones") (Male) (Age 31) (Height 71) (Weight 175)

    putStrLn (patientSummary les)