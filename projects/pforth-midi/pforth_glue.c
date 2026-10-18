/* pforth_glue.c - pForth calls for the CLAP plugin (pforth_clap.cpp), which
 * keeps pForth's internal headers out of C++. */

#include "pf_all.h"
#include "pforth_glue.h"

#include <string.h>

int pfm_glue_init(void) {
    ExecToken entry = 0;
    pfSetQuiet(1);
    return pfInitialize(NULL, 0, &entry) == 0 ? 0 : -1;
}

void pfm_glue_term(void) {
    pfTerminate();
}

/* As QUIT does after an error (pfResetForthTask, static in pf_core.c) */
static void reset_task(void) {
    gCurrentTask->td_InputStream = PF_STDIN;
    gCurrentTask->td_StackPtr = gCurrentTask->td_StackBase;
    gCurrentTask->td_ReturnPtr = gCurrentTask->td_ReturnBase;
#ifdef PF_SUPPORT_FP
    gCurrentTask->td_FloatStackPtr = gCurrentTask->td_FloatStackBase;
#endif
    gCurrentTask->td_IN = gCurrentTask->td_SourceNum;
    gVarState = 0;
}

int pfm_glue_eval_line(const char* line) {
    char buf[TIB_SIZE + 1];
    size_t len = strlen(line);
    if (len > TIB_SIZE) {
        pfMessage("line longer than 256 characters\n");
        reset_task();
        return -1;
    }
    memcpy(buf, line, len + 1);
    ThrowCode result = pfInterpretText(buf);
    if (result != 0) {
        if (result == THROW_BYE) pfMessage("bye is disabled in the plugin\n");
        else pfReportThrow(result);
        reset_task();
        return -1;
    }
    return 0;
}
