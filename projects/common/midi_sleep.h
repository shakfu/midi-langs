/* midi_sleep.h - waits that --no-sleep turns off */

#ifndef MIDI_SLEEP_H
#define MIDI_SLEEP_H

#ifdef __cplusplus
extern "C" {
#endif

/* Process-wide; set once from the command line */
void midi_set_no_sleep(int on);
int midi_no_sleep(void);

/* Sleep ms milliseconds, or return at once under --no-sleep */
void midi_sleep(int ms);

/* Scheduler timer delay: ms, or 0 under --no-sleep */
int midi_wait_ms(int ms);

#ifdef __cplusplus
}
#endif

#endif /* MIDI_SLEEP_H */
