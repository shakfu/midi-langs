/* Prototypes for the C functions PluginRepl.hs imports, defined in
 * mhs_plugin_runtime.c. The generated mhs_plugin_repl.c calls them without
 * including any header, so the build passes this file with -include. */
#ifndef MHS_PLUGIN_H
#define MHS_PLUGIN_H
#include "mhsffi.h"

char* mhs_plugin_next(void);  /* blocks until a request; NULL quits */
void mhs_plugin_done(void);
value_t mhs_plugin_clock_us(void);
void mhs_plugin_set_clock_us(value_t us);
void mhs_plugin_kill_others(void);

#endif
