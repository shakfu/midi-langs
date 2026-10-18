/* pforth_glue.h - see pforth_glue.c */

#ifndef PFORTH_GLUE_H
#define PFORTH_GLUE_H

#ifdef __cplusplus
extern "C" {
#endif

/* Loads the static dictionary; 0 on success */
int pfm_glue_init(void);
void pfm_glue_term(void);

/* Interprets one line; on an error, reports it and resets the stacks and STATE.
 * Returns 0, or -1 on an error. */
int pfm_glue_eval_line(const char* line);

#ifdef __cplusplus
}
#endif

#endif /* PFORTH_GLUE_H */
