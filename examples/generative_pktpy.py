# Generative music demonstrating pktpy-midi's generator patterns
# Run: ./build/pktpy_midi examples/generative_pktpy.py

import midi

midi.set_tempo(480)  # Fast tempo for quick demo
out = midi.open()

# Pentatonic scale
pentatonic = [midi.c4, midi.d4, midi.e4, midi.g4, midi.a4]

def melody():
    """Simple ascending melody"""
    for pitch in pentatonic:
        for ms in midi.play(out, pitch, midi.mf, midi.sixteenth):
            yield ms

def bass():
    """Bass notes"""
    for ms in midi.play(out, midi.c2, midi.f, midi.quarter):
        yield ms
    for ms in midi.play(out, midi.g2, midi.f, midi.quarter):
        yield ms

def chords():
    """Chord stabs"""
    for ms in midi.play_chord(out, midi.major(midi.c3), midi.mp, midi.eighth):
        yield ms

midi.spawn(melody, "melody")
midi.spawn(bass, "bass")
midi.spawn(chords, "chords")
midi.run()

out.close()
print("Generative piece complete!")
