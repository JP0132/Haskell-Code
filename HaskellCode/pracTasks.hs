type Verb = String
pastTense :: Verb -> String
pastTense verb = verb ++ "ed"

type Weight = Float
type Height = Float
bmi :: Weight -> Height -> Float
bmi w h = w/(h^2)