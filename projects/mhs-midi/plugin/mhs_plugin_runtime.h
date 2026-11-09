/* mhs_plugin_runtime.h - MicroHs on a thread of its own, for mhs_clap.cpp.
 *
 * The runtime starts once per process and runs PluginRepl.hs. Every call
 * below is made from the plugin's evaluator thread, after it installs its
 * midi_sink; a request blocks it until the runtime has served it. */

#ifndef MHS_PLUGIN_RUNTIME_H
#define MHS_PLUGIN_RUNTIME_H

#ifdef __cplusplus
extern "C" {
#endif

/* Starts the runtime if it is not running and waits for the REPL; returns 0 when ready.
 * Output from startup is appended to *out (malloc'd, may be NULL). */
int mhs_rt_start(char** out);

/* Serves one request (see PluginRepl.hs) and returns its output, malloc'd.
 * Interrupts the request while midi_clock_cancelled() is true. */
char* mhs_rt_request(const char* req);

/* Earliest script time, in ms, at which a voice is due; 0 if no voice waits. */
int mhs_rt_next_wake(double* ms);

#ifdef __cplusplus
}
#endif

#endif
