module Sound.Backend.Util where


maxIntNBound :: (Num a, Eq a) => a -> a
maxIntNBound n = case n of
    8  -> 127
    16 -> 32_767
    32 -> 2_147_483_647
    64 -> 9_223_372_036_854_775_807
    _  -> 0


minIntNBound :: (Num a, Eq a) => a -> a
minIntNBound n = case maxIntNBound n of
    0 -> 0
    x -> -1 * (x + 1)
