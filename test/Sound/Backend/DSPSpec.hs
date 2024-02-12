module Sound.Backend.DSPSpec ( spec ) where

import Test.Hspec

import Sound.Backend.DSP
import Sound.Backend.State ( initState, State(..) )
import Sound.Backend.Util
import Sound.Notes


quantizeTimeSpec :: Spec
quantizeTimeSpec = describe "quantizeTime" $ do
    it "rounds the time to the nearest interval" $ do
        quantizeTime 0.3 4 `shouldBe` 0.25
        quantizeTime 0.25 4 `shouldBe` 0.25
        quantizeTime 0.2 4 `shouldBe` 0.25
        quantizeTime 0.12500000001 4 `shouldBe` 0.25
        quantizeTime 0 10000000000 `shouldBe` 0
    it "rounds down when halfway between two intervals" $ do
        quantizeTime 0.25 2 `shouldBe` 0
        quantizeTime 0.125 4 `shouldBe` 0


sampleF32toS16Spec :: Spec
sampleF32toS16Spec = describe "sampleF32toS16" $ do
    it "maps a floating-point sample to an s16 sample" $ do
        sampleF32toS16 0.0 `shouldBe` 0
        sampleF32toS16 1.0 `shouldBe` maxIntNBound 16
        sampleF32toS16 (-1.0) `shouldBe` minIntNBound 16 + 1
        sampleF32toS16 0.5 `shouldBe` 16_383
        sampleF32toS16 (-0.67234) `shouldBe` -22_030


sampleWaveformSpec :: Spec
sampleWaveformSpec = describe "sampleWaveform" $ do
    let testPoints = map (/ 440.0) [-1, -0.5, -0.333, 0, 0.125, 0.25, 0.75, 1]
    it "always returns 0.0 when the note state is off" $ do
        let tests = [ sampleWaveform sin NoteOff 7
                    , sampleWaveform (signum . sin) NoteOff 4
                    , sampleWaveform (const 1.0) NoteOff (-1) ]
        mapM_ (\t -> mapM (\p -> t p `shouldBe` 0.0) testPoints) tests
    it "returns the value of the wave function given frequency & time" $ do
        let sW f = sampleWaveform f (NoteOn A) 4
        let check f p = sW f p `shouldBe` f (880.0 * pi * p)
        mapM_ (check sin) testPoints
        mapM_ (check $ signum . sin) testPoints


genSamplesSpec :: Spec
genSamplesSpec = describe "genSamples" $ do
    it "generates the correct number of samples" $ do
        length (genSamples 1_000 initState 1)    `shouldBe` 1_000
        length (genSamples 16_000 initState 1)   `shouldBe` 16_000
        length (genSamples 44_100 initState 1)   `shouldBe` 44_100
        length (genSamples 44_100 initState 0.3) `shouldBe` 13_230
    it "generates the same samples regardless of timing" $ do
        let st t = initState { note = NoteOn C, time = t }
        let gS (t, t') = genSamples 44_100 (st t) t'

        let segmentedSamples = map gS [(0, 0.3), (0.3, 0.7), (0.7, 1.0)]
        let continuousSamples = gS (0, 1)
        concat segmentedSamples `shouldBe` continuousSamples


spec :: Spec
spec = do
    quantizeTimeSpec
    sampleF32toS16Spec
    sampleWaveformSpec
    genSamplesSpec
