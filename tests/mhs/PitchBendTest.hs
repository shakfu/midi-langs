-- PitchBendTest.hs - send pitchBendCents to the aseqdump port
-- test_mhs_midi.sh checks the values aseqdump receives.
module PitchBendTest(main) where

import Data.List (isPrefixOf)
import MusicPerform

main :: IO ()
main = do
    n <- midiListPorts
    names <- mapM midiPortName [0 .. n - 1]
    case [i | (i, s) <- zip [0 ..] names, "aseqdump" `isPrefixOf` s] of
        (i:_) -> do
            _ <- midiOpen i
            pitchBendCents 1 0
            pitchBendCents 1 100
            pitchBendCents 1 (-200)
            midiClose
            putStrLn "sent"
        [] -> putStrLn "no aseqdump port"
