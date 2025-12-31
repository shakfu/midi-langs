-- Pure functional composition demonstrating mhs-midi
-- Run: ./scripts/mhs-midi-repl -r examples/pure_mhs.hs
-- Note: Requires MicroHs setup

import MusicPerform

-- Define a melody using the pure Music DSL
melody :: Music
melody = line [c4, e4, g4, c5] mf sixteenth

-- Define a bass line
bassLine :: Music
bassLine = line [c2, g2] f eighth

-- Main: open MIDI and perform
main :: IO ()
main = do
    midiOpenVirtual "mhs-example"
    spawn "melody" $ perform melody
    spawn "bass" $ perform bassLine
    run
    midiClose
    putStrLn "Done!"
