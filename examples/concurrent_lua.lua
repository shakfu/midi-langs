-- Concurrent voices demonstrating lua-midi's coroutine-based async
-- Run: ./build/lua_midi examples/concurrent_lua.lua

midi.set_tempo(480)  -- Fast tempo for quick demo
open()

-- Melody voice
spawn(function()
    local melody = {c4, e4, g4, c5, g4, e4, c4}
    for _, p in ipairs(melody) do
        play(p, mf, sixteenth)
    end
end, "melody")

-- Bass voice
spawn(function()
    local bass = {c2, c2, g2, c2}
    for _, p in ipairs(bass) do
        play(p, f, eighth)
    end
end, "bass")

-- Pad voice
spawn(function()
    play_chord(major(c3), mp, quarter)
    play_chord(major(g2), mp, quarter)
end, "pad")

run()
close()
print("Done!")
