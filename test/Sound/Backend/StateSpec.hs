module Sound.Backend.StateSpec ( spec ) where

import Data.Time.Clock
import Test.Hspec

import Sound.Backend.Events
import Sound.Backend.State
import Sound.Notes


initStateSpec :: Spec
initStateSpec = describe "initState" $ do
    it "doesn't start with a note playing" $ do
        note initState `shouldBe` NoteOff
    it "starts at the 4th octave" $ do
        octave initState `shouldBe` 4


handleEventSpec :: Spec
handleEventSpec = describe "handleEvent" $ do
    let st = initState
    let time0 = UTCTime { utctDay = toEnum 0, utctDayTime = 0 }
    let _TE = TimedEvent (addUTCTime 1 time0)

    it "always updates the time" $ do
        let events = [ Noop, NoteSet (NoteOn A), NoteSet NoteOff, OctaveSet 1
                     , OctaveDown, OctaveUp ]
        mapM_ (\ev -> time (handleEvent time0 st (_TE ev)) `shouldBe` 1) events
    it "properly increments and decrements octave value" $ do
        octave (handleEvent time0 st (_TE OctaveUp)) `shouldBe` 5
        octave (handleEvent time0 st (_TE OctaveDown)) `shouldBe` 3
    it "properly sets note and octave values" $ do
        note (handleEvent time0 st (_TE $ NoteSet (NoteOn A))) `shouldBe` NoteOn A
        note (handleEvent time0 st (_TE $ NoteSet NoteOff)) `shouldBe` NoteOff
        octave (handleEvent time0 st (_TE $ OctaveSet 1)) `shouldBe` 1
    it "only updates time when event is Noop" $ do
        handleEvent time0 st (TimedEvent time0 Noop) `shouldBe` st


spec :: Spec
spec = do
    initStateSpec
    handleEventSpec
