module Sound.Backend.WaveFunction where


data WaveFunction = Sine | Triangle | Square | Sawtooth | Noise
    deriving (Show, Eq)


infixl 5 /\/
(/\/) :: Floating a => WaveFunction -> a -> a
waveFn /\/ x = case waveFn of
    Sine -> sin x
    Square -> signum (sin x)
    _ -> 0.0
