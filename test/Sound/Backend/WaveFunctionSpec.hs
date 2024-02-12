module Sound.Backend.WaveFunctionSpec ( spec ) where

import Test.Hspec

import Sound.Backend.WaveFunction


waveOperatorSpec :: Spec
waveOperatorSpec = describe "(/\\/)" $ do
    let testPoints = [ -2 * pi, -pi, -pi / 2, -1 / 2, 0, pi / 6 , pi / 2
                     , pi, 2 * pi, 4 * pi ] :: [Float]

    it "calculates accurate values for each WaveFunction" $ do
        map (Sine /\/) testPoints `shouldBe` map sin testPoints
        map (Square /\/) testPoints `shouldBe` map (signum . sin) testPoints
        map (Noise /\/) testPoints `shouldBe` map (const 0.0) testPoints

    it "looks cool" $ True `shouldBe` True


spec :: Spec
spec = do
    waveOperatorSpec
