module MyGIS.Data.Step where


import           MyGIS.Data.Store
import           MyGIS.Data.Dimension
import           MyGIS.Data.Units


type U = Dimensionless Double

type Step1  s d = s d U -> DimIx d -> IO (Src s d U)
type Step2  s d = s d U -> Step1 s d
type Step3  s d = s d U -> Step2 s d
type Step4  s d = s d U -> Step3 s d
type Step5  s d = s d U -> Step4 s d
type Step6  s d = s d U -> Step5 s d
type Step7  s d = s d U -> Step6 s d
type Step8  s d = s d U -> Step7 s d
type Step9  s d = s d U -> Step8 s d
type Step10 s d = s d U -> Step9 s d
