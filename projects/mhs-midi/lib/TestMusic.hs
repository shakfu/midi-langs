-- | Unit tests for pure generative functions in Music.hs
--
-- Run with: mhs-midi -C -i<path> TestMusic
--
module TestMusic(main) where

import Music

-- Simple test framework
data TestResult = Pass String | Fail String String

runTest :: String -> Bool -> TestResult
runTest name True  = Pass name
runTest name False = Fail name "assertion failed"

runTestEq :: (Eq a, Show a) => String -> a -> a -> TestResult
runTestEq name expected actual
    | expected == actual = Pass name
    | otherwise = Fail name ("expected " ++ show expected ++ " but got " ++ show actual)

printResult :: TestResult -> IO ()
printResult (Pass name) = putStrLn $ "  PASS: " ++ name
printResult (Fail name msg) = putStrLn $ "  FAIL: " ++ name ++ " - " ++ msg

isPassing :: TestResult -> Bool
isPassing (Pass _) = True
isPassing (Fail _ _) = False

-----------------------------------------------------------
-- PRNG Tests
-----------------------------------------------------------

testPRNG :: [TestResult]
testPRNG =
    [ runTest "nextRandom is deterministic"
        (fst (nextRandom 42) == fst (nextRandom 42))

    , runTestEq "nextRandom seed 0"
        0 (fst (nextRandom 0))

    , runTestEq "nextRandom seed 42"
        19081 (fst (nextRandom 42))

    , runTest "nextRandom produces different values for different seeds"
        (fst (nextRandom 1) /= fst (nextRandom 2))

    , runTestEq "randomRange lo=hi returns lo"
        5 (fst (randomRange 42 5 5))

    , runTest "randomRange stays in bounds"
        (let (r, _) = randomRange 42 0 10 in r >= 0 && r <= 10)

    , runTest "randomRange multiple calls stay in bounds"
        (all (\s -> let (r, _) = randomRange s 0 10 in r >= 0 && r <= 10) [1..100])

    , runTestEq "randomList length"
        5 (length (fst (randomList 42 5 0 10)))

    , runTest "randomList all in bounds"
        (all (\x -> x >= 0 && x <= 100) (fst (randomList 42 10 0 100)))
    ]

-----------------------------------------------------------
-- Euclidean Rhythm Tests
-----------------------------------------------------------

testEuclidean :: [TestResult]
testEuclidean =
    [ runTestEq "euclideanRhythm 0 8 (no hits)"
        [False, False, False, False, False, False, False, False]
        (euclideanRhythm 0 8)

    , runTestEq "euclideanRhythm 8 8 (all hits)"
        [True, True, True, True, True, True, True, True]
        (euclideanRhythm 8 8)

    , runTestEq "euclideanRhythm 1 4"
        [True, False, False, False]
        (euclideanRhythm 1 4)

    , runTestEq "euclideanRhythm 2 4"
        [True, False, True, False]
        (euclideanRhythm 2 4)

    , runTestEq "euclideanRhythm 3 8 (tresillo)"
        [True, False, False, True, False, False, True, False]
        (euclideanRhythm 3 8)

    , runTestEq "euclideanRhythm 5 8 (cinquillo)"
        [True, False, True, True, False, True, True, False]
        (euclideanRhythm 5 8)

    , runTestEq "euclideanRhythm hit count"
        3 (length (filter id (euclideanRhythm 3 8)))

    , runTestEq "euclideanRhythm total length"
        8 (length (euclideanRhythm 3 8))
    ]

-----------------------------------------------------------
-- Arpeggio Pattern Tests
-----------------------------------------------------------

testArpeggio :: [TestResult]
testArpeggio =
    [ runTestEq "arpUp identity"
        [1, 2, 3, 4] (arpUp [1, 2, 3, 4])

    , runTestEq "arpDown reverse"
        [4, 3, 2, 1] (arpDown [1, 2, 3, 4])

    , runTestEq "arpUpDown"
        [1, 2, 3, 4, 3, 2, 1] (arpUpDown [1, 2, 3, 4])

    , runTestEq "arpUpDown singleton"
        [1] (arpUpDown [1])

    , runTestEq "arpUpDown empty"
        [] (arpUpDown ([] :: [Int]))

    , runTestEq "arpUpDown two elements"
        [1, 2, 1] (arpUpDown [1, 2])
    ]

-----------------------------------------------------------
-- Retrograde Tests
-----------------------------------------------------------

testRetrograde :: [TestResult]
testRetrograde =
    [ runTest "retrograde reverses sequence"
        (collectEvents (retrograde (line [c4, e4, g4] mf quarter))
         == collectEvents (line [g4, e4, c4] mf quarter))

    , runTest "retrograde of retrograde is identity"
        (collectEvents (retrograde (retrograde (line [c4, e4, g4] mf quarter)))
         == collectEvents (line [c4, e4, g4] mf quarter))

    , runTest "retrograde preserves parallel"
        (duration (retrograde (chord [c4, e4, g4] mf quarter))
         == duration (chord [c4, e4, g4] mf quarter))
    ]

-----------------------------------------------------------
-- Invert Tests
-----------------------------------------------------------

testInvert :: [TestResult]
testInvert =
    [ runTestEq "invert c4 around c4 is c4"
        [ENote c4 mf quarter]
        (collectEvents (invert c4 (note c4 mf quarter)))

    , runTestEq "invert e4 around c4"
        [ENote 56 mf quarter]  -- c4 (60) - (e4 (64) - c4 (60)) = 56
        (collectEvents (invert c4 (note e4 mf quarter)))

    , runTestEq "invert g4 around c4"
        [ENote 53 mf quarter]  -- c4 (60) - (g4 (67) - c4 (60)) = 53
        (collectEvents (invert c4 (note g4 mf quarter)))

    , runTest "double inversion is identity"
        (collectEvents (invert c4 (invert c4 (note e4 mf quarter)))
         == collectEvents (note e4 mf quarter))
    ]

-----------------------------------------------------------
-- Shuffle Tests
-----------------------------------------------------------

testShuffle :: [TestResult]
testShuffle =
    [ runTestEq "shuffle preserves length"
        4 (length (shuffle 42 [1, 2, 3, 4]))

    , runTest "shuffle is deterministic"
        (shuffle 42 [1, 2, 3, 4] == shuffle 42 [1, 2, 3, 4])

    , runTest "different seeds produce different results"
        (shuffle 1 [1, 2, 3, 4, 5, 6, 7, 8] /= shuffle 2 [1, 2, 3, 4, 5, 6, 7, 8])

    , runTestEq "shuffle empty"
        [] (shuffle 42 ([] :: [Int]))

    , runTestEq "shuffle singleton"
        [1] (shuffle 42 [1])

    , runTest "shuffle contains all elements"
        (let xs = [1..10]; ys = shuffle 42 xs in all (`elem` ys) xs)
    ]

-----------------------------------------------------------
-- Pick Tests
-----------------------------------------------------------

testPick :: [TestResult]
testPick =
    [ runTest "pick returns element from list"
        (pick 42 [1, 2, 3, 4, 5] `elem` [1, 2, 3, 4, 5])

    , runTest "pick is deterministic"
        (pick 42 [1, 2, 3, 4, 5] == pick 42 [1, 2, 3, 4, 5])

    , runTestEq "pickN returns correct count"
        5 (length (pickN 42 5 [1, 2, 3]))

    , runTest "pickN all elements from list"
        (all (`elem` [1, 2, 3]) (pickN 42 10 [1, 2, 3]))
    ]

-----------------------------------------------------------
-- Random Walk Tests
-----------------------------------------------------------

testRandomWalk :: [TestResult]
testRandomWalk =
    [ runTestEq "randomWalk returns correct count"
        8 (length (randomWalk 42 c4 3 8))

    , runTest "randomWalk starts at start pitch"
        (head (randomWalk 42 c4 3 8) == c4)

    , runTest "randomWalk is deterministic"
        (randomWalk 42 c4 3 8 == randomWalk 42 c4 3 8)

    , runTest "randomWalk stays in MIDI range"
        (all (\p -> p >= 0 && p <= 127) (randomWalk 42 c4 12 100))

    , runTestEq "randomWalk with 0 count"
        [] (randomWalk 42 c4 3 0)
    ]

-----------------------------------------------------------
-- Drunk Walk Tests
-----------------------------------------------------------

testDrunkWalk :: [TestResult]
testDrunkWalk =
    [ let scale = buildScale c4 scaleMajor
      in runTestEq "drunkWalk returns correct count"
            8 (length (drunkWalk 42 c4 scale 2 8))

    , let scale = buildScale c4 scaleMajor
      in runTest "drunkWalk stays on scale"
            (all (`elem` scale) (drunkWalk 42 c4 scale 2 16))

    , let scale = buildScale c4 scaleMajor
      in runTest "drunkWalk is deterministic"
            (drunkWalk 42 c4 scale 2 8 == drunkWalk 42 c4 scale 2 8)

    , runTestEq "drunkWalk with empty scale"
        [] (drunkWalk 42 c4 [] 2 8)

    , runTestEq "drunkWalk with 0 count"
        [] (drunkWalk 42 c4 [c4, e4, g4] 2 0)
    ]

-----------------------------------------------------------
-- Main
-----------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Music.hs Pure Generative Tests ==="
    putStrLn ""

    putStrLn "PRNG Tests:"
    mapM_ printResult testPRNG

    putStrLn ""
    putStrLn "Euclidean Rhythm Tests:"
    mapM_ printResult testEuclidean

    putStrLn ""
    putStrLn "Arpeggio Pattern Tests:"
    mapM_ printResult testArpeggio

    putStrLn ""
    putStrLn "Retrograde Tests:"
    mapM_ printResult testRetrograde

    putStrLn ""
    putStrLn "Invert Tests:"
    mapM_ printResult testInvert

    putStrLn ""
    putStrLn "Shuffle Tests:"
    mapM_ printResult testShuffle

    putStrLn ""
    putStrLn "Pick Tests:"
    mapM_ printResult testPick

    putStrLn ""
    putStrLn "Random Walk Tests:"
    mapM_ printResult testRandomWalk

    putStrLn ""
    putStrLn "Drunk Walk Tests:"
    mapM_ printResult testDrunkWalk

    putStrLn ""
    let allTests = testPRNG ++ testEuclidean ++ testArpeggio ++ testRetrograde
                   ++ testInvert ++ testShuffle ++ testPick ++ testRandomWalk
                   ++ testDrunkWalk
        passed = length (filter isPassing allTests)
        total = length allTests
    putStrLn $ "=== Results: " ++ show passed ++ "/" ++ show total ++ " passed ==="

    if passed == total
        then putStrLn "ALL TESTS PASSED"
        else putStrLn "SOME TESTS FAILED"
