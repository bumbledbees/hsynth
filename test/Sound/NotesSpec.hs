module Sound.NotesSpec ( spec ) where

import Data.Fixed

import Test.Hspec

import Sound.Notes


noteSpec :: Spec
noteSpec = describe "Note" $ do
    it "has C as the first note" $ do
        fromEnum C `shouldBe` 0
        fromEnum B `shouldBe` 11
    it "represents a range of two octaves" $ do
        fromEnum C2 `shouldBe` 12
        fromEnum B2 `shouldBe` 23
    it "properly implements Eq and Ord" $ do
        A < Bb `shouldBe` True
        C < A  `shouldBe` True
        C == C `shouldBe` True


noteStateSpec :: Spec
noteStateSpec = describe "NoteState" $ do
    it "properly implements Eq" $ do
        NoteOn C == NoteOff  `shouldBe` False
        NoteOn A == NoteOn B `shouldBe` False
        NoteOn A == NoteOn A `shouldBe` True
        NoteOff == NoteOff   `shouldBe` True


noteToPitchSpec :: Spec
noteToPitchSpec = describe "noteToPitch" $ do
    it "maps note + octave to pitch with 2 decimal places of precision" $ do
        _ntp A 4  `shouldBe` 440.0
        _ntp A2 3 `shouldBe` 440.0
        _ntp C 5  `shouldBe` 523.25
        _ntp C 0  `shouldBe` 16.35
        _ntp Gb 4 `shouldBe` 369.99 
    where _ntp :: Note -> Octave -> Centi  -- truncate to 2 decimal places
          _ntp n o = fromRational $ toRational $ noteToPitch n o


spec :: Spec
spec = do
    noteSpec
    noteStateSpec
    noteToPitchSpec
