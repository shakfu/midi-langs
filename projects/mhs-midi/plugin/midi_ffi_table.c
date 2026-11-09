/* The MIDI FFI table, renamed: mhs_plugin_runtime.c merges it with the plugin's
 * own entries into the runtime's xffi_table. */
#define xffi_table mhs_midi_ffi_table
#include "midi_ffi_wrappers.c"
