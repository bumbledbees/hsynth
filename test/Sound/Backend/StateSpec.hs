module Sound.Backend.StateSpec ( spec ) where

import Control.Monad.State hiding (evalState)
import Data.Time.Clock
import Test.Hspec

import Sound.Backend.DSP
import Sound.Backend.Environment
import Sound.Backend.Events
import Sound.Backend.State
import Sound.Notes


time0 :: UTCTime
time0 = UTCTime { utctDay = toEnum 0, utctDayTime = 0 }


initStateSpec :: Spec
initStateSpec = describe "initState" $ do
    let st = initState time0
    it "doesn't start with a note playing" $ do
        note st `shouldBe` NoteOff
    it "starts at the 4th octave" $ do
        octave st `shouldBe` 4


nextStateSpec :: Spec
nextStateSpec = describe "nextState" $ do
    let st = initState time0
    let time1 = addUTCTime (secondsToNominalDiffTime 1) time0
    let _TE = TimedEvent time1

    it "always updates the time" $ do
        let events = [ Noop, NoteSet (NoteOn A), NoteSet NoteOff, OctaveSet 1
                     , OctaveDown, OctaveUp ]
        mapM_ (\ev -> time (nextState st (_TE ev)) `shouldBe` time1) events
    it "properly increments and decrements octave value" $ do
        octave (nextState st (_TE OctaveUp)) `shouldBe` 5
        octave (nextState st (_TE OctaveDown)) `shouldBe` 3
    it "properly sets note and octave values" $ do
        note (nextState st (_TE $ NoteSet (NoteOn A))) `shouldBe` NoteOn A
        note (nextState st (_TE $ NoteSet NoteOff)) `shouldBe` NoteOff
        octave (nextState st (_TE $ OctaveSet 1)) `shouldBe` 1
    it "only updates time when event is Noop" $ do
        nextState st (TimedEvent time0 Noop) `shouldBe` st


evalStateSpec :: Spec
evalStateSpec = describe "evalState" $ do
    let st = initState time0
    let env = initEnv time0
    let BackendState { waveFn, octave } = st
    let Environment { sampleRate } = env
    let time1 = addUTCTime (secondsToNominalDiffTime 1) time0

    it "updates the internal state" $ do
        let event = TimedEvent time1 (NoteSet (NoteOn C))
        st' <- execStateT (evalState env event) st
        st' `shouldBe` nextState st event
    it "generates samples based on the internal state" $ do
        let st' = st { note = NoteOn C }
        let event = TimedEvent time1 Noop
        samples <- evalStateT (evalState env event) st'
        samples `shouldBe` genSamples sampleRate waveFn (NoteOn C) octave 0 1
    it "generates the same samples regardless of event timing" $ do
        let st' = st { note = NoteOn C }
        let time03 = addUTCTime (secondsToNominalDiffTime 0.3) time0
        let time07 = addUTCTime (secondsToNominalDiffTime 0.4) time03
        let time10 = addUTCTime (secondsToNominalDiffTime 0.3) time07
        let event = TimedEvent time10 Noop
        let events = map (`TimedEvent` Noop) [time03, time07, time10]
        let actions = map (evalState env) events
        continuousSamples <- evalStateT (evalState env event) st'
        segmentedSamples <- evalStateT (sequence actions) st'
        concat segmentedSamples `shouldBe` continuousSamples


spec :: Spec
spec = do
    initStateSpec
    nextStateSpec
    evalStateSpec
