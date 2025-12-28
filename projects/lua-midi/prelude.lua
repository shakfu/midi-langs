-- Pitch constants
for oct = 0, 8 do
  midi['c'..oct] = 12 + oct * 12
  midi['cs'..oct] = 13 + oct * 12
  midi['d'..oct] = 14 + oct * 12
  midi['ds'..oct] = 15 + oct * 12
  midi['e'..oct] = 16 + oct * 12
  midi['f'..oct] = 17 + oct * 12
  midi['fs'..oct] = 18 + oct * 12
  midi['g'..oct] = 19 + oct * 12
  midi['gs'..oct] = 20 + oct * 12
  midi['a'..oct] = 21 + oct * 12
  midi['as'..oct] = 22 + oct * 12
  midi['b'..oct] = 23 + oct * 12
  -- Flat aliases
  midi['db'..oct] = 13 + oct * 12
  midi['eb'..oct] = 15 + oct * 12
  midi['gb'..oct] = 18 + oct * 12
  midi['ab'..oct] = 20 + oct * 12
  midi['bb'..oct] = 22 + oct * 12
end

-- Dynamics (velocity values)
midi.ppp = 16
midi.pp = 33
midi.p = 49
midi.mp = 64
midi.mf = 80
midi.f = 96
midi.ff = 112
midi.fff = 127

-- Duration constants (ms at 120 BPM)
midi.whole = 2000
midi.half = 1000
midi.quarter = 500
midi.eighth = 250
midi.sixteenth = 125

-- Tempo state
midi._tempo = 120

function midi.set_tempo(bpm)
  midi._tempo = bpm
  local q = math.floor(60000 / bpm)
  midi.quarter = q
  midi.whole = q * 4
  midi.half = q * 2
  midi.eighth = math.floor(q / 2)
  midi.sixteenth = math.floor(q / 4)
end

function midi.get_tempo()
  return midi._tempo
end

function midi.bpm(tempo)
  return math.floor(60000 / tempo)
end

function midi.dotted(dur)
  return math.floor(dur * 1.5)
end

function midi.rest(dur)
  midi.sleep(dur or midi.quarter)
end

-- Global aliases for concise syntax
-- Pitch constants as globals
for oct = 0, 8 do
  _G['c'..oct] = midi['c'..oct]
  _G['cs'..oct] = midi['cs'..oct]
  _G['d'..oct] = midi['d'..oct]
  _G['ds'..oct] = midi['ds'..oct]
  _G['e'..oct] = midi['e'..oct]
  _G['f'..oct] = midi['f'..oct]
  _G['fs'..oct] = midi['fs'..oct]
  _G['g'..oct] = midi['g'..oct]
  _G['gs'..oct] = midi['gs'..oct]
  _G['a'..oct] = midi['a'..oct]
  _G['as'..oct] = midi['as'..oct]
  _G['b'..oct] = midi['b'..oct]
  _G['db'..oct] = midi['db'..oct]
  _G['eb'..oct] = midi['eb'..oct]
  _G['gb'..oct] = midi['gb'..oct]
  _G['ab'..oct] = midi['ab'..oct]
  _G['bb'..oct] = midi['bb'..oct]
end

-- Dynamics as globals
ppp, pp, p, mp, mf, f, ff, fff = midi.ppp, midi.pp, midi.p, midi.mp, midi.mf, midi.f, midi.ff, midi.fff

-- Durations as globals
whole, half, quarter, eighth, sixteenth = midi.whole, midi.half, midi.quarter, midi.eighth, midi.sixteenth

-- Chord builders as globals
major, minor, dim, aug, dom7, maj7, min7 = midi.major, midi.minor, midi.dim, midi.aug, midi.dom7, midi.maj7, midi.min7

-- Other utilities as globals
transpose, octave_up, octave_down = midi.transpose, midi.octave_up, midi.octave_down
dotted, rest, sleep = midi.dotted, midi.rest, midi.sleep

-- Global help function
function help() midi.help() end

-- REPL convenience: global midi output
midi._out = nil

function open(arg)
  if midi._out then midi._out:close() end
  midi._out = midi.open(arg)
  return midi._out
end

function close()
  if midi._out then
    midi._out:close()
    midi._out = nil
  end
end

function n(pitch, vel, dur, ch)
  if not midi._out then error('No MIDI port open. Use open() first.') end
  midi._out:note(pitch, vel or midi.mf, dur or midi.quarter, ch or 1)
end

function ch(pitches, vel, dur, channel)
  if not midi._out then error('No MIDI port open. Use open() first.') end
  midi._out:chord(pitches, vel or midi.mf, dur or midi.quarter, channel or 1)
end

function arp(pitches, vel, dur, channel)
  if not midi._out then error('No MIDI port open. Use open() first.') end
  midi._out:arpeggio(pitches, vel or midi.mf, dur or midi.eighth, channel or 1)
end
