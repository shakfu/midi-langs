/* mhs_plugin_runtime.c - see mhs_plugin_runtime.h.
 *
 * This file compiles the MicroHs runtime (eval_vfs.c, the VFS build of eval.c)
 * with three changes, made here and in plugin/runtime/ rather than in the
 * vendored source:
 *   - Haskell's stdout and stderr write to a private stream, read per request,
 *     instead of the host's;
 *   - no SIGINT handler: Stop sets has_sigint directly;
 *   - the runtime clock is script time (plugin/runtime/extra.c). */

#if !defined(__APPLE__)
#define _GNU_SOURCE /* fopencookie */
#endif
#include <math.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "midi_clock.h"
#include "mhs_plugin_runtime.h"
#include "vfs.h"

static FILE* plugin_out;
#undef stdout
#undef stderr
#define stdout plugin_out
#define stderr plugin_out
#define signal(sig, handler) ((void)(sig), (void)(handler), SIG_DFL)
#define sigprocmask(how, set, old) ((void)(how), (void)(set), (void)(old), 0)
#include "eval_vfs.c"
#undef stdout
#undef stderr
#undef signal
#undef sigprocmask

/* --- request handoff ------------------------------------------------------ */

static pthread_mutex_t g_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_cond = PTHREAD_COND_INITIALIZER;
static char* g_request;     /* owned; NULL asks the REPL to quit */
static int g_has_request;   /* a request is waiting for the runtime */
static int g_done;          /* the runtime finished the last request */
static int g_busy;          /* the runtime is serving a request */
static int g_started;       /* the runtime thread exists */
static int g_exited;        /* mhs_main returned */
static char* g_out;         /* output since the last request was posted */
static size_t g_out_len, g_out_cap;

static int write_out(void* cookie, const char* buf, int n)
{
	(void)cookie;
	pthread_mutex_lock(&g_mutex);
	if (g_out_len + (size_t)n + 1 > g_out_cap) {
		size_t cap = g_out_cap ? g_out_cap : 4096;
		while (cap < g_out_len + (size_t)n + 1) cap *= 2;
		char* grown = realloc(g_out, cap);
		if (grown) {
			g_out = grown;
			g_out_cap = cap;
		}
	}
	if (g_out_len + (size_t)n + 1 <= g_out_cap) {
		memcpy(g_out + g_out_len, buf, (size_t)n);
		g_out_len += (size_t)n;
		g_out[g_out_len] = '\0';
	}
	pthread_mutex_unlock(&g_mutex);
	return n;
}

#if !defined(__APPLE__)
static ssize_t write_out_cookie(void* cookie, const char* buf, size_t n)
{
	return write_out(cookie, buf, (int)n);
}
#endif

char* mhs_plugin_next(void)
{
	pthread_mutex_lock(&g_mutex);
	while (!g_has_request) pthread_cond_wait(&g_cond, &g_mutex);
	g_has_request = 0;
	g_busy = g_request != NULL;
	char* req = g_request;
	pthread_mutex_unlock(&g_mutex);
	return req;
}

void mhs_plugin_done(void)
{
	pthread_mutex_lock(&g_mutex);
	g_busy = 0;
	g_done = 1;
	pthread_cond_broadcast(&g_cond);
	pthread_mutex_unlock(&g_mutex);
}

/* Takes the output so far; the caller frees it. */
static char* take_output(void)
{
	char* out = g_out ? g_out : strdup("");
	g_out = NULL;
	g_out_len = g_out_cap = 0;
	return out;
}

/* Waits for the runtime to finish. While Stop is pending, interrupts it once:
 * UserInterrupt goes to the REPL thread, which ends the request. */
static void wait_done_locked(void)
{
	int interrupted = 0;
	while (!g_done && !g_exited) {
		struct timespec until;
		clock_gettime(CLOCK_REALTIME, &until);
		until.tv_nsec += 5 * 1000 * 1000;
		if (until.tv_nsec >= 1000000000L) {
			until.tv_sec += 1;
			until.tv_nsec -= 1000000000L;
		}
		pthread_cond_timedwait(&g_cond, &g_mutex, &until);
		if (!interrupted && g_busy && midi_clock_cancelled()) {
			has_sigint = true;
			interrupted = 1;
		}
	}
	g_done = 0;
}

/* --- the clock and the voices, called on the runtime thread ---------------- */

int64_t mhs_plugin_clock_get(void)
{
	return (int64_t)llround(midi_clock_ms() * 1000.0);
}

/* Every thread sleeps: advance script time to the earliest wake. In Live mode
 * the sink's wait holds this until the host has rendered up to it. */
void mhs_plugin_clock_sleep(useconds_t usecs)
{
	(void)usecs;
	/* Without IO polling nothing else delivers Stop to a runtime that mostly
	 * sleeps: its threads wake, run briefly and sleep again before a time slice
	 * ends, which is where the runtime checks has_sigint. */
	if (has_sigint) {
		check_sigint();
		return;
	}
	if (timeq.mq_head) midi_clock_wait_until(timeq.mq_head->mt_at / 1000.0);
}

value_t mhs_plugin_clock_us(void)
{
	return (value_t)mhs_plugin_clock_get();
}

void mhs_plugin_set_clock_us(value_t us)
{
	midi_clock_set_ms(us / 1000.0);
}

void mhs_plugin_kill_others(void)
{
	for (struct mthread* mt = all_threads; mt; mt = mt->mt_next) {
		if (mt->mt_id != MAIN_THREAD && mt->mt_state != ts_finished && mt->mt_state != ts_died)
			async_throwto(mt, mkInt(exn_threadkilled));
	}
}

/* --- FFI ------------------------------------------------------------------ */

/* The generated PluginRepl wrappers (mhs_plugin_repl.c, whose own table the build
 * strips) and the MIDI table (midi_ffi_wrappers.c, built with xffi_table renamed). */
extern from_t mhs_mhs_plugin_next(int);
extern from_t mhs_mhs_plugin_done(int);
extern from_t mhs_mhs_plugin_clock_us(int);
extern from_t mhs_mhs_plugin_set_clock_us(int);
extern from_t mhs_mhs_plugin_kill_others(int);
extern const struct ffi_entry* mhs_midi_ffi_table;

static const struct ffi_entry plugin_ffi[] = {
	{"mhs_plugin_next", 0, mhs_mhs_plugin_next},
	{"mhs_plugin_done", 0, mhs_mhs_plugin_done},
	{"mhs_plugin_clock_us", 0, mhs_mhs_plugin_clock_us},
	{"mhs_plugin_set_clock_us", 1, mhs_mhs_plugin_set_clock_us},
	{"mhs_plugin_kill_others", 0, mhs_mhs_plugin_kill_others},
};

const struct ffi_entry* xffi_table;

static void build_ffi_table(void)
{
	size_t nplugin = sizeof(plugin_ffi) / sizeof(plugin_ffi[0]);
	size_t nmidi = 0;
	while (mhs_midi_ffi_table[nmidi].ffi_name) nmidi++;
	struct ffi_entry* table = calloc(nplugin + nmidi + 1, sizeof(struct ffi_entry)); /* lives for the process */
	memcpy(table, plugin_ffi, sizeof(plugin_ffi));
	memcpy(table + nplugin, mhs_midi_ffi_table, nmidi * sizeof(struct ffi_entry));
	xffi_table = table;
}

/* --- startup -------------------------------------------------------------- */

static void* run_runtime(void* arg)
{
	(void)arg;
	static char* argv[] = {"mhs-midi", "-a/mhs-embedded", "-pbase", "-pmusic", NULL};
	mhs_main(4, argv);
	pthread_mutex_lock(&g_mutex);
	g_exited = 1;
	pthread_cond_broadcast(&g_cond);
	pthread_mutex_unlock(&g_mutex);
	return NULL;
}

int mhs_rt_start(char** out)
{
	pthread_mutex_lock(&g_mutex);
	if (g_started) {
		int ok = !g_exited;
		pthread_mutex_unlock(&g_mutex);
		if (out) *out = strdup("");
		return ok ? 0 : -1;
	}
	g_started = 1;
	pthread_mutex_unlock(&g_mutex);

	vfs_init();
	setenv("MHSDIR", vfs_get_temp_dir(), 1);
	build_ffi_table();
#if defined(__APPLE__)
	plugin_out = funopen(NULL, NULL, write_out, NULL, NULL);
#else
	cookie_io_functions_t io = {NULL, write_out_cookie, NULL, NULL};
	plugin_out = fopencookie(NULL, "w", io);
#endif
	setvbuf(plugin_out, NULL, _IONBF, 0);

	/* The REPL thread lives as long as the process; it is never joined. */
	pthread_t thread;
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setstacksize(&attr, 64 << 20);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	int failed = pthread_create(&thread, &attr, run_runtime, NULL);
	pthread_attr_destroy(&attr);

	pthread_mutex_lock(&g_mutex);
	if (failed) g_exited = 1;
	else wait_done_locked();
	char* text = take_output();
	int ok = !g_exited;
	pthread_mutex_unlock(&g_mutex);
	if (out) *out = text;
	else free(text);
	return ok ? 0 : -1;
}

char* mhs_rt_request(const char* req)
{
	pthread_mutex_lock(&g_mutex);
	if (g_exited) {
		pthread_mutex_unlock(&g_mutex);
		return strdup("mhs-midi: the interpreter has stopped\n");
	}
	free(g_request);
	g_request = strdup(req);
	free(g_out);
	g_out = NULL;
	g_out_len = g_out_cap = 0;
	g_done = 0;
	g_has_request = 1;
	pthread_cond_broadcast(&g_cond);
	wait_done_locked();
	char* out = take_output();
	pthread_mutex_unlock(&g_mutex);
	return out;
}

int mhs_rt_next_wake(double* ms)
{
	/* The runtime is idle between requests, so its queues hold still. */
	pthread_mutex_lock(&g_mutex);
	int found = 0;
	for (struct mthread* mt = runq.mq_head; mt && !found; mt = mt->mt_queue) {
		if (mt->mt_id != MAIN_THREAD) {
			*ms = midi_clock_ms(); /* runnable now */
			found = 1;
		}
	}
	if (!found && timeq.mq_head) {
		*ms = timeq.mq_head->mt_at / 1000.0;
		found = 1;
	}
	pthread_mutex_unlock(&g_mutex);
	return found;
}
