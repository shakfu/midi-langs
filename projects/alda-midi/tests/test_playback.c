/**
 * @file test_playback.c
 * @brief Tests for alda-midi's playback timing helpers.
 */

#include "test_framework.h"
#include <alda/scheduler.h>

/* 40000 ticks is about 83 beats; ticks * 60000 exceeds INT_MAX. */
TEST(ticks_to_ms_long_gap) {
    ASSERT_EQ(alda_ticks_to_ms(40000, 120), 41666);
}

TEST(ms_to_ticks_long_gap) {
    ASSERT_EQ(alda_ms_to_ticks(60000, 120), 57600);
}

TEST(ticks_to_ms_short) {
    ASSERT_EQ(alda_ticks_to_ms(ALDA_TICKS_PER_QUARTER, 120), 500);
    ASSERT_EQ(alda_ticks_to_ms(0, 120), 0);
}

BEGIN_TEST_SUITE("Playback timing")
    RUN_TEST(ticks_to_ms_long_gap);
    RUN_TEST(ms_to_ticks_long_gap);
    RUN_TEST(ticks_to_ms_short);
END_TEST_SUITE()
