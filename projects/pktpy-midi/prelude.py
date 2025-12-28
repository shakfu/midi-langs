import midi
import time as _time

# ============================================================================
# Dynamics (velocity values)
# ============================================================================
midi.ppp = 16
midi.pp = 33
midi.p = 49
midi.mp = 64
midi.mf = 80
midi.f = 96
midi.ff = 112
midi.fff = 127

# ============================================================================
# Durations (milliseconds at 120 BPM)
# ============================================================================
midi.whole = 2000
midi.half = 1000
midi.quarter = 500
midi.eighth = 250
midi.sixteenth = 125

def _dotted(duration):
    '''Return dotted duration (1.5x)'''
    return int(duration * 1.5)
midi.dotted = _dotted

# ============================================================================
# Tempo
# ============================================================================
midi._bpm = 120

def _set_tempo(bpm):
    '''Set tempo in BPM and update duration constants'''
    midi._bpm = bpm
    beat_ms = 60000 // bpm
    midi.quarter = beat_ms
    midi.half = beat_ms * 2
    midi.whole = beat_ms * 4
    midi.eighth = beat_ms // 2
    midi.sixteenth = beat_ms // 4
midi.set_tempo = _set_tempo

def _get_tempo():
    '''Get current tempo in BPM'''
    return midi._bpm
midi.get_tempo = _get_tempo

def _bpm_to_ms(tempo):
    '''Return quarter note duration in ms for given tempo'''
    return 60000 // tempo
midi.bpm = _bpm_to_ms

# ============================================================================
# Timing helpers
# ============================================================================
def _sleep(ms):
    '''Sleep for given milliseconds'''
    _time.sleep(ms / 1000.0)
midi.sleep = _sleep

def _rest(duration=None):
    '''Rest (silence) for given duration, default is quarter note'''
    if duration is None:
        duration = midi.quarter
    _time.sleep(duration / 1000.0)
midi.rest = _rest

# ============================================================================
# Chord builders (return list of MIDI pitches)
# ============================================================================
def _major(root):
    '''Build major triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 7]
midi.major = _major

def _minor(root):
    '''Build minor triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 3, root + 7]
midi.minor = _minor

def _dim(root):
    '''Build diminished triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 3, root + 6]
midi.dim = _dim

def _aug(root):
    '''Build augmented triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 8]
midi.aug = _aug

def _dom7(root):
    '''Build dominant 7th chord from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 7, root + 10]
midi.dom7 = _dom7

def _maj7(root):
    '''Build major 7th chord from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 7, root + 11]
midi.maj7 = _maj7

def _min7(root):
    '''Build minor 7th chord from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 3, root + 7, root + 10]
midi.min7 = _min7

# ============================================================================
# Pitch helpers
# ============================================================================
def _transpose(pitch, semitones):
    '''Transpose pitch by semitones'''
    if isinstance(pitch, str):
        pitch = midi.note(pitch)
    return pitch + semitones
midi.transpose = _transpose

def _octave_up(pitch):
    '''Transpose pitch up one octave'''
    return _transpose(pitch, 12)
midi.octave_up = _octave_up

def _octave_down(pitch):
    '''Transpose pitch down one octave'''
    return _transpose(pitch, -12)
midi.octave_down = _octave_down

# ============================================================================
# Pitch constants (all octaves)
# ============================================================================
for _oct in range(9):
    setattr(midi, f'c{_oct}', 12 + _oct * 12)
    setattr(midi, f'cs{_oct}', 13 + _oct * 12)
    setattr(midi, f'db{_oct}', 13 + _oct * 12)
    setattr(midi, f'd{_oct}', 14 + _oct * 12)
    setattr(midi, f'ds{_oct}', 15 + _oct * 12)
    setattr(midi, f'eb{_oct}', 15 + _oct * 12)
    setattr(midi, f'e{_oct}', 16 + _oct * 12)
    setattr(midi, f'f{_oct}', 17 + _oct * 12)
    setattr(midi, f'fs{_oct}', 18 + _oct * 12)
    setattr(midi, f'gb{_oct}', 18 + _oct * 12)
    setattr(midi, f'g{_oct}', 19 + _oct * 12)
    setattr(midi, f'gs{_oct}', 20 + _oct * 12)
    setattr(midi, f'ab{_oct}', 20 + _oct * 12)
    setattr(midi, f'a{_oct}', 21 + _oct * 12)
    setattr(midi, f'as{_oct}', 22 + _oct * 12)
    setattr(midi, f'bb{_oct}', 22 + _oct * 12)
    setattr(midi, f'b{_oct}', 23 + _oct * 12)
del _oct

# ============================================================================
# CC helpers (common control changes)
# ============================================================================
midi.CC_MODULATION = 1
midi.CC_BREATH = 2
midi.CC_VOLUME = 7
midi.CC_PAN = 10
midi.CC_EXPRESSION = 11
midi.CC_SUSTAIN = 64
midi.CC_REVERB = 91
midi.CC_CHORUS = 93

# ============================================================================
# MidiOut extensions
# ============================================================================
def _arpeggio(self, pitches, velocity=80, note_duration=None, spacing=None, channel=1):
    '''Play notes sequentially (arpeggiated)'''
    if note_duration is None:
        note_duration = midi.eighth
    if spacing is None:
        spacing = note_duration
    for p in pitches:
        self.note(p, velocity, note_duration, channel)
        if spacing > note_duration:
            midi.sleep(spacing - note_duration)
midi.MidiOut.arpeggio = _arpeggio

def _modulation(self, value, channel=1):
    '''Set modulation wheel (CC 1)'''
    self.cc(1, value, channel)
midi.MidiOut.modulation = _modulation

def _volume(self, value, channel=1):
    '''Set channel volume (CC 7)'''
    self.cc(7, value, channel)
midi.MidiOut.volume = _volume

def _pan(self, value, channel=1):
    '''Set pan position (CC 10): 0=left, 64=center, 127=right'''
    self.cc(10, value, channel)
midi.MidiOut.pan = _pan

def _sustain(self, on=True, channel=1):
    '''Set sustain pedal (CC 64)'''
    self.cc(64, 127 if on else 0, channel)
midi.MidiOut.sustain = _sustain
