-- Compiled by mhs_standalone_compile_midi: a program that sends MIDI must link
-- the MIDI libraries, including midi_open (midi_clock.c, midi_sleep.c).
module TestCompileMidi(main) where

import MidiPerform

main :: IO ()
main = do
    open
    note c4
    chord [c4, e4, g4]
    close
